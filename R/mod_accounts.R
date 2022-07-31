#' accounts UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList



# create_unique_ids <- function(n, seed_no = 1, char_len = 5){
#   set.seed(seed_no)
#   pool <- c(letters, LETTERS, 0:9)
#
#   res <- character(n) # pre-allocating vector is much faster than growing it
#   for(i in seq(n)){
#     this_res <- paste0(sample(pool, char_len, replace = TRUE), collapse = "")
#     while(this_res %in% res){ # if there was a duplicate, redo
#       this_res <- paste0(sample(pool, char_len, replace = TRUE), collapse = "")
#     }
#     res[i] <- this_res
#   }
#   res
# }

create_unique_ids <- function(n = 1, char_len = 5){
  pool <- c(letters, LETTERS, 0:9)

  res <- character(n)
  for(i in seq(n)){

    this_res <- paste0(sample(pool, char_len, replace = TRUE), collapse = "")
    # as long as n = 1 then wont need to worry about dupicates (like in 'while')
    # though !!! there should be a duplicate check, after prefixing username[1:4]
    # in write RDS and write to DB, with a rerun of create_unique_ids if duplicate
    # is found already on on file
    res[i] <- this_res
  }
  res
}




mod_accounts_ui <- function(id){
  ns <- NS(id)
  tagList(
    bs4Dash::tabBox(
      title = "Accounts"
      , elevation = 2
      , id = "tabcard1"
      , width = 12
      , collapsible = FALSE
      , closable = FALSE
      , type = "tabs"
      , status = "primary"
      , solidHeader = TRUE
      , selected = "accounts"
      , side = "right"
      , tabPanel(
        "accounts"
        , fluidRow(
          mod_accounts_tab_ui(ns("accounts_tab_ui_1"))
        )
      )
      , tabPanel(
        "users"
        , fluidRow(
          bs4Dash::tabsetPanel(
            id = "tabsetpanel3",
            selected = "on file",
            vertical = TRUE,
            tabPanel(
              "on file"
              , bs4Dash::tabBox(
                title = 'on file'
                , elevation = 2
                , id = ns("tb_onfl")
                , width = 12
                , collapsible = FALSE
                , closable = FALSE
                , type = "tabs"
                , status = "primary"
                , solidHeader = TRUE
                , selected = "userbase"
                , side = "right"
                , tabPanel(
                  "userbase"
                  , fluidRow(
                    column(3
                           , align = 'center'
                           , br()
                           , actionButton(ns('btn_user_rfrsh'), 'Refresh', width = '80%')
                    )
                    , column(9
                             , align = 'center'
                             , br()
                             , bs4Dash::infoBox(
                               title      = "User Base"
                               , color    = "primary"
                               , gradient = TRUE
                               , fill     = TRUE
                               , width    = 12
                               , icon = shiny::icon("fas fa-chart-bar")
                               , tableOutput(ns('outpt_user_base'))
                             )
                    )
                  )
                )
                # , tabPanel(
                #   "userbase rds"
                #   , fluidRow(
                #     column(3
                #            , align = 'center'
                #            , br()
                #            , actionButton(ns('btn_user_rds_rfrsh'), 'Refresh', width = '80%')
                #     )
                #     , column(9
                #              , align = 'center'
                #              , br()
                #              , infoBox(
                #                title      = "User Base"
                #                , color    = "primary"
                #                , gradient = TRUE
                #                , fill     = TRUE
                #                , width    = 12
                #                , tableOutput(ns('outpt_user_rds_base'))
                #              )
                #     )
                #   )
                # )
                , tabPanel(
                  "profiles"
                  , fluidRow(
                    column(3
                           , align = 'center'
                           , br()
                           , selectInput(ns('slt_user_profile')
                                         , label = NULL
                                         , choices = NULL
                           )
                    )
                    , column(9
                           , align = 'left'
                           , br()
                           , bs4Dash::infoBox(
                             title      = h5(tags$u("User Profile"))
                             , color    = "primary"
                             , gradient = TRUE
                             , fill     = TRUE
                             , width    = 12
                             , icon = shiny::icon("fas fa-chart-bar")
                             , uiOutput(ns('outpt_accounts_users'))
                             ,
                           )
                    )

                  )
                )
              )
            )
            , tabPanel(
              "add users"
              , bs4Dash::tabBox(
                title = 'add users',
                elevation = 2,
                id = "tabcard1",
                width = 12,
                collapsible = FALSE,
                closable = FALSE,
                type = "tabs",
                status = "primary",
                solidHeader = TRUE,
                selected = "form",
                side = "right",
                tabPanel(
                  "form"
                  , fluidRow(
                    tags$style(type="text/css"  , paste0("#", ns("txt_add_user {color: white}")))
                    , tags$style(type="text/css"  , paste0("#", ns("txt_add_name {color: white}")))
                    , tags$style(type="text/css"  , paste0("#", ns("txt_add_surname_name {color: white}")))
                    , tags$style(type="text/css"  , paste0("#", ns("txt_add_email {color: white}")))
                    , tags$style(type="text/css"  , paste0("#", ns("txt_add_user {color: white}")))
                    , tags$style(type="text/css"  , paste0("#", ns("txt_add_cell {color: white}")))
                    , tags$style(type="text/css"  , paste0("#", ns("txt_add_password {color: white}")))
                    , column(6
                             , align = 'center'
                             , textInput(ns('txt_add_user'), 'Username', width = '80%')
                             , textInput(ns('txt_add_name'), 'Given Name', width = '80%')
                             , textInput(ns('txt_add_surname_name'), 'Surname', width = '80%')
                             , textInput(ns('txt_add_email'), 'Email', width = '80%')
                             , textInput(ns('txt_add_cell'), 'Cell', width = '80%')


                    )
                    , column(6
                             , align = 'center'
                             , selectInput(
                               ns('slt_permission')
                               , 'Permission'
                               , choices = c('admin','standard')
                               , selected = 'standard'
                               , width = '80%'
                             )
                             , selectInput(ns('slt_add_status')
                                           , 'Status'
                                           , choices = c('active', 'inactive')
                                           , selected = 'active'
                                           , width = '80%'
                             )
                             , uiOutput(ns('outpt_slt_add_account'))
                             , textInput(ns('txt_add_password'), 'Password', width = '80%')
                             , br()
                             , actionButton(ns('btn_add_user'), 'Commit', width = '80%')
                    )
                  )
                )
              )
            )
            , tabPanel(
              "edit users"
              , fluidRow(
                column(12
                       , mod_edit_users_ui(ns("edit_users_ui_1"))
                )
              )
            )
          )
        )
      )

    )

  )
}

#' accounts Server Functions
#'
#' @noRd
mod_accounts_server <- function(
  id
  , user_base
  , callback
  # , logout_init
){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


    #### <<<<    CALLMODULES     >>>>  ####
    #-------------------------------------#

    mod_edit_users_server(
      "edit_users_ui_1"
      , btn_cnfrm_add_user_rctv = reactive({input$btn_cnfrm_add_user_rctv})
    )

    mod_accounts_tab_server(
      "accounts_tab_ui_1"
      , callback = callback
    )

    #### <<<<    STATIC VALUES   >>>>  ####
    #-------------------------------------#


    #### <<<<   REACTIVES        >>>>  ####
    #-------------------------------------#


    user_base_rctv <- reactive({

      input$btn_user_rfrsh

      con <- appbench::database_connection()

      sql_results <- DBI::dbGetQuery(
        con
        , 'select * from accounts.user_base'
      ) %>%
        dplyr::select(user, name)

      DBI::dbDisconnect(con)

      return(sql_results)

    })


    # add_account_choices_rctv <- reactive({
    #
    #   DBI::dbGetQuery(
    #     con
    #     , "select distinct (account) from accounts.accounts order by account;"
    #   ) %>%
    #     dplyr::pull(account)
    #
    # })


    #### <<<<   REACTIVES VALS   >>>>  ####
    #-------------------------------------#

    profile_fks_rctv <- reactiveVal(NULL)


    #### <<<<   EVENT REACTIVES  >>>>  ####
    #-------------------------------------#


    #### <<<<   OBSERVES         >>>>  ####
    #-------------------------------------#


    #### <<<<   OBSERVE EVENTS   >>>>  ####
    #-------------------------------------#


    observeEvent(input$slt_user_profile,{



      sql_line <- paste0('select * from accounts.users where "user" = '
                         , paste0("'", input$slt_user_profile, "';")
      )

      con <- appbench::database_connection()

      sql_resulst <- profile_fks_rctv(

        DBI::dbGetQuery(con, sql_line)
      )

      DBI::dbDisconnect(con)

    })


    observeEvent(input$tb_onfl, {

      con <- appbench::database_connection()

      users <- DBI::dbGetQuery(con,
                               'select distinct "user" from accounts.users order by "user"'
      ) %>%
        dplyr::pull(user)

      DBI::dbDisconnect(con)

      updateSelectInput(
        session
        , 'slt_user_profile'
        , label    = NULL
        , choices  = users
        , selected = users[1]
      )

    })


    observeEvent(input$btn_add_user, {

      # user_check <- readRDS('R/user_base.rds')$user

      # FUTURE DEV should refactor this to check inside the DB
      # and set the conditional response on the sql return value

      con <- appbench::database_connection()

      user_check <- DBI::dbGetQuery(
        con
        , 'select * from accounts.user_base'
      ) %>%
        dplyr::pull(user)

      DBI::dbDisconnect(con)

      # print('user_check')
      # print(user_check)
      # print('-------------user_check')

      if(input$txt_add_user == ''){

        showModal(modalDialog(
          title = "Please enter a username!"
          , easyClose = TRUE
          , footer = NULL
          , br()
        ))

      } else if(input$txt_add_name == ''){

        showModal(modalDialog(
          title = "Please enter a name!"
          , easyClose = TRUE
          , footer = NULL
          , br()
        ))

      } else if(input$txt_add_password == ''){

        showModal(modalDialog(
          title = "Please enter a password!"
          , easyClose = TRUE
          , footer = NULL
          , br()
        ))

      } else if(input$txt_add_user %in% user_check){

        showModal(modalDialog(
          title = "username taken - please enter a different username!"
          , easyClose = TRUE
          , footer = NULL
          , br()

        ))


      } else {

        showModal(modalDialog(
          title = "Confirm ADD NEW USER"
          , easyClose = TRUE
          , footer = NULL
          , br()
          , uiOutput(ns('outpt_cnfrm_add_user'))
        ))

      }

    })





    observeEvent(input$btn_cnfrm_add_user, {

      user_id <- paste0(stringr::str_sub(input$txt_add_user, 1,4), '_', create_unique_ids(1))

      # print(user_id)

      user_tbl <- tibble::tibble(
        user_id         = user_id
        , user          = input$txt_add_user
        , password      = input$txt_add_password
        , password_hash = purrr::map_chr(input$txt_add_password, sodium::password_store)
        , name          = input$txt_add_name
        , permissions   = input$slt_permission
      )

      # updt_user_base <- readRDS('R/user_base.rds')
      # updt_user_base <- updt_user_base %>% dplyr::bind_rows(user_tbl)
      # readr::write_rds(updt_user_base, 'R/user_base.rds')



      table_id <- DBI::Id(
        schema  = "accounts"
        , table   = 'user_base'
      )

      con <- appbench::database_connection()

      DBI::dbWriteTable(
        con
        , name      = table_id
        , value     = user_tbl
        , row.names = F
        , append    = T
        , overwrite = F
      )


      time_stamp          <- Sys.time()

      # NB - there is a rename to given_name here - future dev to standarsise with rds!
      users <- user_tbl %>%
        dplyr::rename(given_name = name) %>%
        dplyr::select(-c(password, password_hash)) %>%
        dplyr::mutate(time_stamp   = time_stamp) %>%
        dplyr::mutate(surname_name = input$txt_add_surname_name) %>% #
        dplyr::mutate(status       = input$slt_add_status) %>% #
        dplyr::mutate(account      = input$slt_add_account) %>% #
        dplyr::mutate(email        = input$txt_add_email) %>% #
        dplyr::mutate(cell         = input$txt_add_cell)



      table_id <- DBI::Id(
        schema  = "accounts"
        , table   = 'users'
      )

      DBI::dbWriteTable(
        con
        , name      = table_id
        , value     = users
        , row.names = F
        , append    = T
        , overwrite = F
      )

      # user_base2 <- DBI::dbGetQuery(con, 'SELECT * FROM accounts.user_base')


      # credentials <- shinyauthr::loginServer(
      #   id            = "login",
      #   data          = user_base2,
      #   user_col      = user,
      #   pwd_col       = password_hash,
      #   sodium_hashed = TRUE,
      #   cookie_logins = TRUE,
      #   sessionid_col = sessionid,
      #   cookie_getter = get_sessions_from_db,
      #   cookie_setter = add_session_to_db,
      #   log_out       = reactive(logout_init())
      # )


      DBI::dbDisconnect(con)

      # con <- appbench::database_connection()
      # DBI::dbDisconnect(con)

      removeModal()

    })


    #### <<<<    OUTPUTS         >>>>  ####
    #-------------------------------------#



    output$outpt_accounts_users <- renderUI({

      req(profile_fks_rctv())

      x <- profile_fks_rctv()

      tagList(
        h6(glue::glue('User: {profile_fks_rctv()$user}'))
        , h6(glue::glue('Name: {profile_fks_rctv()$given_name}'))
        , h6(glue::glue('Surname: {profile_fks_rctv()$surname_name}'))
        , h6(glue::glue('Account: {profile_fks_rctv()$account}'))
        , h6(glue::glue('Email: {profile_fks_rctv()$email}'))
        , h6(glue::glue('Cell: {profile_fks_rctv()$cell}'))
        , h6(glue::glue('Permission: {profile_fks_rctv()$permissions}'))
      )

    })


    output$outpt_cnfrm_add_user <- renderUI({

      tagList(
        h4(paste('You are about to add ', input$txt_add_name, ' with username '
                 , input$txt_add_user, ' and permission status ', input$slt_permission
                 , 'to the user base!'
        ))
        , br()
        , actionButton(ns('btn_cnfrm_add_user'), 'CONFIRM')
      )

    })


    # output$outpt_slt_add_account <- renderUI({
    #
    #   selectInput(ns('slt_add_account')
    #               , 'Account'
    #               , choices = add_account_choices_rctv()
    #               , selected = NULL
    #               , width = '80%'
    #   )
    # })



    output$outpt_user_base <- renderTable({

      user_base_rctv()
      # %>%
      #   dplyr::select(-password)

    })


    # output$outpt_user_rds_base <- renderTable({
    #
    #   user_base_rctv()
    #
    # })



  })
}

## To be copied in the UI
# mod_accounts_ui("accounts_ui_1")

## To be copied in the server
# mod_accounts_server("accounts_ui_1")
