#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd

cookie_expiry <- 7

# This is a FIX from SO - LOST the link
renderMenu <- function(expr, env = parent.frame(), quoted = FALSE, outputArgs = list()) {
  if (!quoted) {
    expr <- substitute(expr)
    quoted <- TRUE
  }
  shiny::renderUI(expr, env = env, quoted = quoted, outputArgs = outputArgs)
}


get_sessions_from_db <- function(
    # conn = con,
    expiry = cookie_expiry) {
  # DBI::dbReadTable(conn, "sessions") %>%
  #   dplyr::mutate(login_time = lubridate::ymd_hms(login_time)) %>%
  #   tibble::as_tibble() %>%
  #   dplyr::filter(login_time > lubridate::now() - lubridate::days(expiry))

  # print('FIRED')

  conn <- DBI::dbConnect(
    RPostgres::Postgres()
    , dbname   = R.utils::getOption("db.dev.dbname")
    , host     = R.utils::getOption("db.dev.host")
    , port     = R.utils::getOption("db.dev.port")
    , user     = R.utils::getOption("db.dev.user")
    , password = R.utils::getOption("db.dev.password")
  )

  get_sessions <- DBI::dbGetQuery(
    conn
    , glue::glue("select * from accounts.sessions")
  )

  get_sessions <- dplyr::mutate(get_sessions, login_time = lubridate::ymd_hms(login_time))
  get_sessions <- dplyr::filter(get_sessions, login_time > lubridate::now() - lubridate::days(expiry))

  DBI::dbDisconnect(conn)

  return(get_sessions)

}


add_session_to_db <- function(user, sessionid
                              # , conn = con
                              ) {

  session_tbl <- tibble::tibble(
    user         = user
    , sessionid  = sessionid
    , login_time = as.character(lubridate::now())
    )

  val_user  <- session_tbl$user
  val_id    <- session_tbl$sessionid
  val_login <- session_tbl$login_time

  sql_line <- paste0(glue::glue('insert into accounts.sessions ("user", sessionid, login_time)
                 values ')
                     , glue::glue("('{val_user}','{val_id}','{val_login}')
                 ")
  )

  connB <- DBI::dbConnect(
    RPostgres::Postgres()
    , dbname   = R.utils::getOption("db.dev.dbname")
    , host     = R.utils::getOption("db.dev.host")
    , port     = R.utils::getOption("db.dev.port")
    , user     = R.utils::getOption("db.dev.user")
    , password = R.utils::getOption("db.dev.password")
  )


  DBI::dbExecute(
    connB
    , sql_line
  )

  DBI::dbDisconnect(connB)

}



callback_calls <- lapply(0:30, function(i){

  # DataTables length
  select_i <- paste0("$('#DataTables_Table_", i, "_length select').css('background-color', 'white');")
  label_i  <- paste0("$('#DataTables_Table_", i, "_length label').css('color', 'white');")

  # DataTables filter
  label_i2 <- paste0("$('#DataTables_Table_", i, "_filter label').css('color', 'white');")
  input_i  <- paste0("$('#DataTables_Table_", i, "_filter input').css('background-color', 'white');")

  # DataTables previous
  label_i3 <- paste0("$('#DataTables_Table_", i, "_previous').css('color', 'white');")

  # DataTables processing
  label_i4 <- paste0("$('#DataTables_Table_", i, "_processing label').css('color', 'white');")

  # DataTables info
  label_i6 <- paste0("$('#DataTables_Table_", i, "_info label').css('color', 'white');")
  label_i8 <- paste0("$('#DataTables_Table_", i, "_info').css('color', 'white');")

  # DataTables paginate
  label_i9 <- paste0("$('#DataTables_Table_", i, "_paginate').css('background-color', 'white');" )

  all_calls <- c(select_i
                 , input_i
                 , label_i
                 , label_i2
                 , label_i3
                 , label_i4
                 , label_i6
                 , label_i8
                 , label_i9
  )

  return(all_calls)

})

callback <- (unlist(callback_calls))


app_server <- function(input, output, session) {

  #### <<<<    LOGIN SECTION     >>>>  ####
  #-------------------------------------#

  con <- appbench::database_connection()
  user_base <- DBI::dbGetQuery(con, 'SELECT * FROM accounts.user_base')
  DBI::dbDisconnect(con)

  credentials <- shinyauthr::loginServer(
    id            = "login",
    data          = user_base,
    user_col      = user,
    pwd_col       = password_hash,
    sodium_hashed = TRUE,
    cookie_logins = TRUE,
    sessionid_col = sessionid,
    cookie_getter = get_sessions_from_db,
    cookie_setter = add_session_to_db,
    log_out       = reactive(logout_init()),
    reload_on_logout  = T
  )


  logout_init <- shinyauthr::logoutServer(
    id = "logout",
    active = reactive(credentials()$user_auth)
  )

  observe({
    if (credentials()$user_auth) {
      shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
    } else {
      shinyjs::addClass(selector = "body", class = "sidebar-collapse")
    }
  })

  user_info <- reactive({
    credentials()$info
  })

  observeEvent(credentials()$info,{

    # print('credentials()$info')
    # print(credentials()$info %>% dplyr::select(user, name))

    user_name_rctv <- user_name_rctv(credentials()$info$name)

  })


  user_data <- reactive({
    req(credentials()$user_auth)

    if (user_info()$permissions == "admin") {
      dplyr::starwars[, 1:10]
    } else if (user_info()$permissions == "standard") {
      dplyr::storms[, 1:11]
    }
  })

  output$welcome <- renderText({

    req(credentials()$user_auth)
    glue::glue("Welcome {user_info()$name}")

  })


  #### <<<<    GARGOYLE        >>>>  ####
  #-------------------------------------#

  gargoyle::init(
    "account_slt"
    , 'updt_edit_account_choices'
    # , 'sessions_rctv'
    # , 'sessions_rctv2'
    # , 'div_datasets'
    # , 'tgr_sn_xmpl'
    # , 'tgr_delete_sn'
    # , 'trg_edt_sn'
  )


  #### <<<<    CALLMODULES     >>>>  ####
  #-------------------------------------#


  mod_accounts_server(
    "accounts_ui_1"
    , user_base = user_base
    , callback  = callback
    # , logout_init = logout_init
  )

  mod_accounts_tab_server(
    "accounts_tab_ui_1"
    , callback = callback
  )

  mod_enter_hours_server(
    "enter_hours_1"
    , menu_left_main_trigger = reactive({input$menu_left_main})
    , btn_gen_r_8_form       = reactive({input$btn_gen_r_8_form})
    , dstnct_accounts        = reactive({dstnct_accounts()})
    )


  #### <<<<    STATIC VALUES   >>>>  ####
  #-------------------------------------#


  #### <<<<   REACTIVES        >>>>  ####
  #-------------------------------------#


  #### <<<<   REACTIVES VALS   >>>>  ####
  #-------------------------------------#

  # This is needed (with the observeenet on it) to pass the right name to mod_home_server
  user_name_rctv <- reactiveVal(NULL)

  #### <<<<   EVENT REACTIVES  >>>>  ####
  #-------------------------------------#

  dstnct_accounts <- eventReactive(input$btn_gen_r_8_form,{

    db_sql <- 'select distinct account from accounts.accounts order by account;'

    message(db_sql)

    con <- appbench::database_connection()

    rtrnr <- DBI::dbGetQuery(
      con
      , db_sql
    ) %>%
      dplyr::pull(account)

    DBI::dbDisconnect(con)

    return(rtrnr)

  })



  #### <<<<   OBSERVES         >>>>  ####
  #-------------------------------------#


  #### <<<<   OBSERVE EVENTS   >>>>  ####
  #-------------------------------------#

  observeEvent(credentials()$info,{

    # print('credentials()$info')
    # print(credentials()$info %>% dplyr::select(user, name))

    user_name_rctv <- user_name_rctv(credentials()$info$name)

  })


  #### <<<<    OUTPUTS         >>>>  ####
  #-------------------------------------#

  #### <<<<    OUTPUT OPTIONS  >>>>  ####
  #-------------------------------------#

  output$outpt_main <- renderUI({
    req(credentials()$user_auth)

    bs4Dash::bs4TabItems(
      bs4Dash::bs4TabItem(
        'view_accounts'
        , mod_accounts_ui("accounts_ui_1")
      )
      , bs4Dash::bs4TabItem(
        'enter_hours'
        , mod_enter_hours_ui("enter_hours_1")
      )



    )

  })



  output$outpt_sidebar <- renderMenu({

    req(credentials()$user_auth)

    if(credentials()$info$permissions == 'admin'){

      menu_side <- bs4Dash::sidebarMenu(
        id = 'menu_left_main'
        , .list = NULL
        , flat = FALSE
        , compact = FALSE
        , childIndent = TRUE
        , legacy = FALSE
        , bs4Dash::bs4SidebarMenuItem(
          'Admin'
          , icon = NULL
          , badgeLabel = NULL
          , badgeColor = "success"
          , tabName = 'view_admin'
          , href = NULL
          , newTab = NULL
          , selected = T
          , startExpanded = T
          , condition = NULL
          , bs4Dash::menuSubItem(
            "Accounts"
            , tabName = "view_accounts"
            , icon = icon("circle")
          )
          , bs4Dash::menuSubItem(
            "Enter Hours"
            , tabName = "enter_hours"
            # , conditionalPanel("input.menu_left_main === 'enter_hours'",
            #                    'help'
            #                  # sliderInput("b", "Under sidebarMenu", 1, 100, 50)
            # )
            # , selectInput(
            #   'slt_test'
            #   , 'slt_test'
            #   , choices = letters
            # )
            , icon = icon("circle")
            , selected = T
          )
          , conditionalPanel("input.menu_left_main === 'enter_hours'"
                             , dateInput(
                               'side_dt_entr_day'
                               , 'Select Date'
                               , value = lubridate::today()
                             )
                             , actionButton("btn_gen_r_8_form", "GENR8 a Record", width = '100%')
                             , br()
                             , br()
                             , actionButton("btn_write_rds", "write_rds", width = '100%')
                             , br()
                             , br()
                             , actionButton("btn_clear_records", "clear", width = '100%')

                             # sliderInput("b", "Under sidebarMenu", 1, 100, 50)
          )

          # , bs4Dash::bs4SidebarMenuSubItem(
          #   "Enter Hours"
          #   , tabName = "enter_hours"
          #   , icon = icon("circle")
          #   , selected = T
          #   # , selectInput(
          #   #   'slt_test'
          #   #   , 'slt_test'
          #   #   , choices = letters
          #   # )
          # )

          # , dateInput(
          #   'side_dt_entr_day'
          #   , 'Select Date'
          #   , value = lubridate::today()
          # )

        )
      )


    }
    # else {
    #
    #   menu_side <- bs4Dash::sidebarMenu(
    #     id = 'menu_left_main',
    #     .list = NULL,
    #     flat = FALSE,
    #     compact = FALSE,
    #     childIndent = TRUE,
    #     legacy = FALSE
    #   )
    #
    #
    # }

    return(menu_side)

  })



}


