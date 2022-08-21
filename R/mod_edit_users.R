#' edit_users UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_edit_users_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      tags$style(type="text/css"  , paste0("#", ns("txt_edit_user {color: white}")))
      , tags$style(type="text/css", paste0("#", ns("txt_edit_name {color: white}")))
      , tags$style(type="text/css", paste0("#", ns("txt_edit_password {color: white}")))
      , tags$style(type="text/css", paste0("#", ns("txt_edit_surname {color: white}")))
      , tags$style(type="text/css", paste0("#", ns("txt_edit_email {color: white}")))
      , tags$style(type="text/css", paste0("#", ns("txt_edit_cell {color: white}")))

      , bs4Dash::tabBox(
        title = 'edit users'
        , elevation = 2
        , id = "tabcard1"
        , width = 12
        , collapsible = FALSE
        , closable = FALSE
        , type = "tabs"
        , status = "primary"
        , solidHeader = TRUE
        , selected = "edit"
        , side = "right"
        , tabPanel(
          "edit"
          , fluidRow(
            column(4
                   , align = 'center'
                   , uiOutput(ns('outpt_slt_edit_user'))
                   , actionButton(ns('btn_refresh_slt_edit_user'), 'Refresh Edit List', width = '80%')
                   , br()
                   , br()
                   , actionButton(ns('btn_fetch_edit_user'), 'Fetch', width = '80%')
                   , br()
                   , br()
                   , actionButton(ns('btn_commit_edit'), 'Commit', width = '80%')
            )
            , column(8
                     , align = 'center'
                     , uiOutput(ns('outpt_edit_user'))
            )
          )
        )
        , tabPanel(
          "delete"
          , fluidRow(
            column(4
                   , align = 'center'
                   , uiOutput(ns('outpt_slt_delete_user'))
                   , actionButton(ns('btn_refresh_slt_delete_user'), 'Refresh', width = '80%')
                   , br()
                   , br()
                   , actionButton(ns('btn_fetch_delete_user'), 'Fetch', width = '80%')
                   , br()
                   , br()
                   , actionButton(ns('btn_commit_delete'), 'Commit', width = '80%')
            )
            , column(8
                     , align = 'center'
                     , div(id = ns('div_delete_user')
                           , br()
                           , uiOutput(ns('outpt_delete_user'), width = 12)
                     )
            )
          )
        )
      )
    )
  )
}

#' edit_users Server Functions
#'
#' @noRd
mod_edit_users_server <- function(
  id
  , btn_cnfrm_add_user_rctv
){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    #### <<<<    CALLMODULES     >>>>  ####
    #-------------------------------------#

    #### <<<<    STATIC VALUES   >>>>  ####
    #-------------------------------------#


    #### <<<<   REACTIVES        >>>>  ####
    #-------------------------------------#

    edit_accounts_rctv <- reactive({

      sql_line <- paste0(
        'select distinct (account) from accounts.accounts order by account'
      )


      con <- appbench::database_connection()

      sql_res <- DBI::dbGetQuery(
        con
        , sql_line
      ) %>%
        dplyr::pull(account)

      DBI::dbDisconnect(con)

      return(sql_res)

    })


    user_edit_users_rctv <- reactive({

      # print("user_edit_users_rctv")

      btn_cnfrm_add_user_rctv()
      input$btn_cnfrm_delete_user
      input$btn_refresh_slt_edit_user
      input$btn_refresh_slt_delete_user

      con <- appbench::database_connection()

      sql_res <- DBI::dbGetQuery(
        con
        , 'select "user" from accounts.user_base order by "user"'
      ) %>%
        dplyr::pull(user)

      DBI::dbDisconnect(con)

      return(sql_res)

    })


    #### <<<<   REACTIVES VALS   >>>>  ####
    #-------------------------------------#


    #### <<<<   EVENT REACTIVES  >>>>  ####
    #-------------------------------------#

    edit_user_rctv <- eventReactive(input$btn_fetch_edit_user,{

      # print('btn pressed')

      # x_userbase <- readRDS('R/user_base.rds') %>%
      #   dplyr::filter(user == input$slt_edit_user)

      x_userbase_sql <- paste0('select * from accounts.user_base where "user" = '
                               , "'"
                               , input$slt_edit_user
                               , "'"
      )

      # print('x_userbase_sql')
      # print(x_userbase_sql)
      # print('---------------')


      con <- appbench::database_connection()

      x_userbase <- DBI::dbGetQuery(
        con
        , x_userbase_sql
      )


      # print('x_userbase')
      # print(x_userbase)

      y_db <- DBI::dbGetQuery(
        con
        , paste0("select * from accounts.users where user_id = '"
                 , x_userbase$user_id
                 , "'"
        )
      )

      DBI::dbDisconnect(con)
      # print('y_db')
      # print(y_db)

      z_joined <- x_userbase %>%
        dplyr::inner_join(y_db)

      # print('z_joined')
      # print(z_joined)

      # print('edit_user_rctv')
      # print(y)

      return(z_joined)
    })


    observeEvent(input$btn_fetch_delete_user,{

      # print('btn_fetch_delete_user')

      shinyjs::show(id = "div_delete_user", asis = F)

      delete_record_check('show')

    })

    delete_user_rctv <- eventReactive(input$btn_fetch_delete_user,{

      # wip point ####

      # print('btn_fetch_delete_user')
      # print(input$btn_fetch_delete_user)

      # x <- readRDS('R/user_base.rds') %>%
      #   dplyr::filter(user == input$slt_delete_user)

      x <- DBI::dbGetQuery(
        con
        , paste0('select * from accounts.user_base where "user" = '
                 , "'"
                 , input$slt_delete_user
                 , "';"
        )
      )

      return(x)
    })


    #### <<<<   OBSERVES         >>>>  ####
    #-------------------------------------#


    #### <<<<   OBSERVE EVENTS   >>>>  ####
    #-------------------------------------#


    observeEvent(input$btn_cnfrm_delete_user,{

      # delete the user from the rds user_base
      # update the status on the database to reflect that the user's login
      # has been removed - i.e. retain the user's profile etc as 'account deactivated'

      # user_deleter <- readRDS('R/user_base.rds') %>%
      #   dplyr::filter(user != delete_user_rctv()$user)

      # user_deleter %>% readr::write_rds('R/user_base.rds')

      sql_line_a <- paste0(
        "delete from accounts.user_base where "
        , '"user"'
        , " = '"
        , delete_user_rctv()$user
        , "';"
      )

      DBI::dbExecute(
        con
        , sql_line_a
      )

      sql_line <- paste0(
        "update accounts.users set status = 'deleted' where "
        , '"user"'
        , " = '"
        , delete_user_rctv()$user
        , "';"
      )

      DBI::dbGetQuery(
        con
        , sql_line
      )

      shinyjs::hide(id='div_delete_user', asis = F)

      # delete_record_check

      updateSelectInput(
        session
        , 'slt_delete_user'
        , 'Select user'
        , choices = user_edit_users_rctv()
      )

      delete_record_check('hide')

      removeModal()

    })


    delete_record_check <- reactiveVal('hide')



    observeEvent(input$btn_commit_delete, {

      # print('delete_record_check()')
      # print(delete_record_check())

      if(delete_record_check() == 'hide'){

        showModal(modalDialog(
          title = "Fetch a User!"
          , easyClose = TRUE
          , footer = NULL
        ))

      } else if(input$btn_fetch_delete_user == 0){

        showModal(modalDialog(
          title = "Fetch a User!"
          , easyClose = TRUE
          , footer = NULL
        ))

      } else {

        showModal(modalDialog(
          title = "Delete User?"
          , easyClose = TRUE
          , footer = NULL
          , br()
          , h5("You are about to delete ", delete_user_rctv()$user, "from the userbase!")
          , br()
          , h5('This will...')
          , h5(paste0('1) COMPLETELY REMOVE ', delete_user_rctv()$user, " from the login userbase, and"))
          , h5("2) adjust ", paste0(delete_user_rctv()$user, "'s permanent database status to 'deleted'."))
          , br()
          , actionButton(ns('btn_cnfrm_delete_user'), 'CONFIRM DELETE USER!')
          , br()
          , br()
        ))
      }
    })



    observeEvent(input$btn_commit_edit, {

      # this should be a method to ensure that it doesn't conflict
      # with another user's unique username, while also allowing for the same
      # user to maintain their existing username

      # user_check <- readRDS('R/user_base.rds') %>%
      #   dplyr::select(user) %>%
      #   dplyr::filter(user != edit_user_rctv()$user) %>%
      #   dplyr::pull(user)


      # FUTURE DEV - Put this check into the DB, not memory


      con <- appbench::database_connection()

      user_check <- DBI::dbGetQuery(
        con
        , paste0('select "user" from accounts.sessions where "user" != '
                 , "'"
                 , edit_user_rctv()$user
                 , "';"
        )
      ) %>%
        dplyr::pull(user)

      DBI::dbDisconnect(con)

      if(input$txt_edit_user == ''){

        showModal(modalDialog(
          title = "Blank "
          , easyClose = TRUE
          , footer = NULL
          , br()
          , h4("Please enter a valid username")
        ))

      } else if(input$txt_edit_name == ''){

        showModal(modalDialog(
          title = "Blank "
          , easyClose = TRUE
          , footer = NULL
          , br()
          , h4("Please enter a valid name")
        ))

      } else if(input$txt_edit_password == ''){

        showModal(modalDialog(
          title = "Blank "
          , easyClose = TRUE
          , footer = NULL
          , br()
          , h4("Please enter a valid password")
        ))

      } else if(input$txt_edit_user %in% user_check){

        showModal(modalDialog(
          title = "username taken - please enter a different username!"
          , easyClose = TRUE
          , footer = NULL
          , br()

        ))

      } else {

        showModal(modalDialog(
          title = "Edit User Details"
          , easyClose = TRUE
          , footer = NULL
          , br()
          , h4("You are about to Update the User Details")
          , br()
          , h5(paste0("UserID "     , edit_user_rctv()$user_id))
          , br()
          , h5(paste0(edit_user_rctv()$user , "'s details will be updated to..." ))
          , h5(paste0('Username: '   , input$txt_edit_user))
          , h5(paste0('Given Name: ' , input$txt_edit_name))
          , h5(paste0('Surname: '    , input$txt_edit_surname))
          , h5(paste0('Email: '      , input$txt_edit_email))
          , h5(paste0('Cell: '       , input$txt_edit_cell))
          , h5(paste0('Permission: ' , input$slt_edit_permission))
          , h5(paste0('Status: '     , input$slt_edit_status))
          , h5(paste0('Account: '    , input$slt_edit_account))
          , h5(paste0('Password: '   , input$txt_edit_password))
          , br()
          , actionButton(ns('btn_cnfrm_edit_user'), 'CONFIRM')
          , br()
          , br()
        ))
      }
    })


    observeEvent(input$btn_cnfrm_edit_user,{

      # ATTN - this process needs confirmation checks that the values
      # will not break DB field integrity

      # edit_user_rctv()$user_id

      user_tbl <- tibble::tibble(
        user_id         = edit_user_rctv()$user_id
        , user          = input$txt_edit_user
        , password      = input$txt_edit_password
        , password_hash = purrr::map_chr(input$txt_edit_password, sodium::password_store)
        , name          = input$txt_edit_name
        , permissions   = input$slt_edit_permission
      )

      # updt_user_base <- readRDS('R/user_base.rds')

      # print('updt_user_base')
      # print(updt_user_base)
      # print('updt_user_base - end --------------')
      # print('updt_user_base')
      # print(edit_user_rctv())
      # print('updt_user_base - end --------------')


      # remove user's previous user_base record
      # updt_user_base <- updt_user_base %>%
      #   dplyr::filter(user_id != edit_user_rctv()$user_id)


      con <- appbench::database_connection()

      DBI::dbExecute(
        con
        , paste0('delete from accounts.user_base where user_id = '
                 , "'"
                 , edit_user_rctv()$user_id
                 , "'"
        )
      )


      # updt_user_base <- updt_user_base %>% dplyr::bind_rows(user_tbl)

      # readr::write_rds(updt_user_base, 'R/user_base.rds')

      table_id <- DBI::Id(
        schema  = "accounts"
        , table   = 'user_base'
      )

      DBI::dbWriteTable(
        con
        , name      = table_id
        , value     = user_tbl
        , row.names = F
        , append    = T
        , overwrite = F
      )

      # time_stamp          <- Sys.time()


      # remove user's previous (DB) users record

      DBI::dbExecute(
        con
        , paste0("delete from accounts.users where user_id = '", edit_user_rctv()$user_id, "';")
      )


      users <- user_tbl %>%
        dplyr::select(-c(password, password_hash)) %>%
        dplyr::rename(given_name    = name) %>%
        dplyr::mutate(given_name    = input$txt_edit_name) %>%
        dplyr::mutate(surname_name  = input$txt_edit_surname) %>%
        dplyr::mutate(status        = input$slt_edit_status) %>%
        dplyr::mutate(account       = input$slt_edit_account) %>%
        dplyr::mutate(email         = input$txt_edit_email) %>%
        dplyr::mutate(cell          = input$txt_edit_cell)

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

      DBI::dbDisconnect(con)

      removeModal()

    })


    #### <<<<    OUTPUTS         >>>>  ####
    #-------------------------------------#


    output$outpt_delete_user <- bs4Dash::renderbs4ValueBox({

      # req(delete_user_rctv())

      bs4Dash::valueBox(
        subtitle = NULL
        , value = tagList(
          h4('User Details')
          , hr()
          , h5(paste("Username: "  , delete_user_rctv()$user))
          , h5(paste("Given Name: ", delete_user_rctv()$name))
        )
        , color = "orange"
        # , icon = shiny::icon("cog", verify_fa = FALSE)
        , icon = shiny::icon('ice-lolly-tasted', lib="glyphicon")
        , width = 12
      )

      # tagList(
      #   br()
      #   , br()
      #   , h4('User Details')
      #   , hr()
      #   , h5(paste("Username: "  , delete_user_rctv()$user))
      #   , h5(paste("Given Name: ", delete_user_rctv()$name))
      #   # , h5(paste("Surname: "   , delete_user_rctv()$surname_name))
      #   # , h5(paste("Status: "    , delete_user_rctv()$status))
      #   # , h5(paste("Account: "   , delete_user_rctv()$account))
      #   # , h5(paste("Email: "     , delete_user_rctv()$email))
      #   # , h5(paste("Cell: "      , delete_user_rctv()$cell))
      #   # , h5(paste("Cell: "      , delete_user_rctv()$cell))
      #   # , h5(paste("Permission: ", delete_user_rctv()$permissions))
      # )
    })



    output$outpt_edit_user <- renderUI({

      req(edit_user_rctv())


      tagList(
        textInput(
          ns('txt_edit_user')
          , label = 'Username'
          , value = edit_user_rctv()$user
          , width = '80%'
          # , style = 'color: yellow;'
        )
        , textInput(
          ns('txt_edit_name')
          , label = 'Given Name'
          , value = edit_user_rctv()$name
          , width = '80%'
        )
        , textInput(
          ns('txt_edit_surname')
          , label = 'Surname'
          , value = edit_user_rctv()$surname_name
          , width = '80%'
        )
        , textInput(
          ns('txt_edit_email')
          , label = 'Email'
          , value = edit_user_rctv()$surname_name
          , width = '80%'
        )
        , textInput(
          ns('txt_edit_cell')
          , label = 'Cell'
          , value = edit_user_rctv()$surname_name
          , width = '80%'
        )
        , selectInput(
          ns('slt_edit_permission')
          , label = 'Permission'
          , choices = c('admin', 'standard')
          , selected = edit_user_rctv()$permissions
          , width = '80%'
        )
        , selectInput(
          ns('slt_edit_status')
          , label = 'Status'
          , choices = c('active', 'inactive')
          , selected = edit_user_rctv()$status
          , width = '80%'
        )
        , selectInput(
          ns('slt_edit_account')
          , label = 'Account'
          , choices = edit_accounts_rctv()
          , selected = edit_user_rctv()$account
          , width = '80%'
        )

        , textInput(
          ns('txt_edit_password')
          , label = 'Password'
          , value = ''
          , placeholder = 'Must add a NEW PASSWORD!'
          , width = '80%'
        )


      )

    })



    output$outpt_slt_delete_user <- renderUI({

      tagList(
        selectInput(ns('slt_delete_user')
                    , 'Select User'
                    , choices = user_edit_users_rctv()
                    , width = '80%'
        )
      )

    })


    output$outpt_slt_edit_user <- renderUI({

      tagList(
        selectInput(ns('slt_edit_user')
                    , 'Select User'
                    , choices = user_edit_users_rctv()
                    , width = '80%'
        )
      )

    })


  })
}


