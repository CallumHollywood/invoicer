#' accounts_tab UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom gargoyle watch


mod_accounts_tab_ui <- function(id){
  ns <- NS(id)
  tagList(
    bs4Dash::tabsetPanel(
      id = "tabsetpanel3",
      selected = "view account",
      vertical = TRUE,
      tabPanel(
        "view account"
        , bs4Dash::tabBox(
          title = 'view account',
          elevation = 2,
          id = "tabcard1",
          width = 12,
          collapsible = FALSE,
          closable = FALSE,
          type = "tabs",
          status = "primary",
          solidHeader = TRUE,
          selected = "account",
          side = "right",
          tabPanel(
            "account"
            , mod_view_account_ui(ns("view_account_ui_1"))
          )
        )
      )
      , tabPanel(
        "on file"
        , bs4Dash::tabBox(
          title = 'on file',
          elevation = 2,
          id = "tabcard1",
          width = 12,
          collapsible = FALSE,
          closable = FALSE,
          type = "tabs",
          status = "primary",
          solidHeader = TRUE,
          selected = "accounts",
          side = "right",
          tabPanel(
            "accounts"
            , fluidRow(
              column(3
                     , align = 'center'
                     , actionButton(ns('btn_refresh_accounts'), 'Refresh', width = '80%')
              )
            )
            , br()
            , fluidRow(
              bs4Dash::tabBox(
                title = 'status',
                elevation = 2,
                id = "tabcard1",
                width = 12,
                collapsible = FALSE,
                closable = FALSE,
                type = "tabs",
                status = "primary",
                solidHeader = TRUE,
                selected = "active",
                side = "right",
                tabPanel(
                  "active"
                  , fluidRow(
                    column(12
                           , DT::dataTableOutput(ns('outpt_accounts_on_file_actv'))
                    )
                  )
                )
                , tabPanel(
                  "inactive"
                  , fluidRow(
                    column(12
                           , DT::dataTableOutput(ns('outpt_accounts_on_file_inactv'))
                    )
                  )
                )

              )

            )
            # , fluidRow(
            #   column(12
            #          , DT::dataTableOutput(ns('outpt_accounts_on_file'))
            #   )
            # )
          )
        )
      )
      , tabPanel(
        "add account"
        , bs4Dash::tabBox(
          title = 'add account',
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
              tags$style(type="text/css"  , paste0("#", ns("txt_add_account {color: white}")))
              , tags$style(type="text/css"  , paste0("#", ns("txt_add_account_details_name {color: white}")))
              , tags$style(type="text/css"  , paste0("#", ns("txt_add_account_details_surname {color: white}")))
              , tags$style(type="text/css"  , paste0("#", ns("txt_add_account_details_email {color: white}")))
              , tags$style(type="text/css"  , paste0("#", ns("txt_add_account_details_cell {color: white}")))
              , column(4
                       , align = 'center'
                       , br()
                       , br()
                       , br()
                       , actionButton(ns('btn_refresh_add_account'), 'Refresh', width = '80%')
                       , br()
                       , br()
                       , actionButton(ns('btn_commit_add_account'), 'Commit', width = '80%')
              )
              , column(4
                       , align = 'center'
                       , h5('Account Details')
                       , br()
                       , textInput(ns('txt_add_account'), 'Account Name')
                       , br()
                       , selectInput(ns('slt_account_status')
                                     , 'Status'
                                     , choices = c('active', 'inactive')
                                     , selected = 'active'
                       )
              )
              , column(4
                       , align = 'center'
                       , h5('Contact Details')
                       , br()
                       , textInput(ns('txt_add_account_details_name'), 'Given Name')
                       , br()
                       , textInput(ns('txt_add_account_details_surname'), 'Surname')
                       , br()
                       , textInput(ns('txt_add_account_details_email'), 'Email')
                       , br()
                       , textInput(ns('txt_add_account_details_cell'), 'Cell No')
              )
            )
          )
        )
      )
      , tabPanel(
        "edit account"
        , bs4Dash::tabBox(
          title = 'edit account',
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
              # tags$style(type="text/css"  , paste0("#", ns("txt_add_account {color: white}")))
              # , tags$style(type="text/css"  , paste0("#", ns("txt_add_account_details_name {color: white}")))
              # , tags$style(type="text/css"  , paste0("#", ns("txt_add_account_details_surname {color: white}")))
              # , tags$style(type="text/css"  , paste0("#", ns("txt_add_account_details_email {color: white}")))
              # , tags$style(type="text/css"  , paste0("#", ns("txt_add_account_details_cell {color: white}")))
              column(4
                     , align = 'center'
                     , br()
                     , br()
                     , uiOutput(ns('outpt_slt_edit_account'))
                     , actionButton(ns('btn_fetch_edit_account'), 'Fetch', width = '80%')
                     , br()
                     , br()
                     , actionButton(ns('btn_commit_edit_account'), 'Commit', width = '80%')
                     , br()
                     , br()
                     # , actionButton(ns('btn_deactivate_account'), 'Deactivate', width = '80%')

              )
              , column(4
                       , align = 'center'
                       , h5('Account Details')
                       , uiOutput(ns('outpt_edit_account_left'))
              )
              , column(4
                       , align = 'center'
                       , h5('Contact Details')
                       , uiOutput(ns('outpt_edit_account_right'))
                       # , textInput(ns('txt_add_account_details_name'), 'Given Name')
                       # , br()
                       # , textInput(ns('txt_add_account_details_surname'), 'Surname')
                       # , br()
                       # , textInput(ns('txt_add_account_details_email'), 'Email')
                       # , br()
                       # , textInput(ns('txt_add_account_details_cell'), 'Cell No')
              )
            )
          )
        )
      )
    )

  )
}

#' accounts_tab Server Functions
#'
#' @noRd
mod_accounts_tab_server <- function(
  id
  , callback
){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    #### <<<<    CALLMODULES     >>>>  ####
    #-------------------------------------#

    mod_view_account_server("view_account_ui_1")

    #### <<<<    STATIC VALUES   >>>>  ####
    #-------------------------------------#


    #### <<<<   REACTIVES        >>>>  ####
    #-------------------------------------#

    # view_account_record_rctv <- reactive({
    #
    #   view_account_rctv() %>%
    #     dplyr::filter(time_stamp == input$slt_account_dates)
    #
    # })


    gargoyle::on('updt_edit_account_choices',{

      con <- appbench::database_connection()

      edit_account_choices <- DBI::dbGetQuery(
        con
        , 'select distinct (account) from accounts.accounts order by account;'
      ) %>%
        dplyr::pull(account)

      DBI::dbDisconnect(con)

      updateSelectInput(
        session
        , 'slt_edit_account'
        , 'Select Account'
        , choices = edit_account_choices
        , selected = edit_account_choices[1]
      )

    })



    edit_account_choices_rctv <- reactive({

      input$btn_commit_edit_account

      con <- appbench::database_connection()


      sql_results <- DBI::dbGetQuery(
        con
        , 'select distinct (account) from accounts.accounts order by account;'
      ) %>%
        dplyr::pull(account)

      DBI::dbDisconnect(con)

      return(sql_results)

    })

    accounts_on_file_rctv <- reactive({

      # refresh
      input$btn_refresh_accounts

      sql_line <- paste0(
        "select *
        from accounts.accounts acnt
        inner join ( select
        account_id
        , max(time_stamp)
        from accounts.accounts
        group by account_id
        ) mx
        on acnt.time_stamp = mx.max
        and acnt.account_id = mx.account_id
         order by account;"
      )

      con <- appbench::database_connection()

      sql_results <- DBI::dbGetQuery(
        con
        , sql_line
      )

      DBI::dbDisconnect(con)

      return(sql_results)

    })


    #### <<<<   REACTIVES VALS   >>>>  ####
    #-------------------------------------#


    #### <<<<   EVENT REACTIVES  >>>>  ####
    #-------------------------------------#



    edit_account_rctv <- eventReactive(input$btn_fetch_edit_account, {

      # sql_line <- paste0(
      #   "select * from accounts.accounts where account = '"
      #   , input$slt_edit_account
      #   , "';"
      # )

      sql_line <- paste0(
        "select *
        from accounts.accounts acnt
        inner join ( select
        account_id
        , max(time_stamp)
        from accounts.accounts
        group by account_id
        ) mx
        on acnt.time_stamp = mx.max
        and acnt.account_id = mx.account_id
        where acnt.account = '", input$slt_edit_account, "';"
      )


      print("sql_line --------")
      print(sql_line)


      con <- appbench::database_connection()

      sql_results <- DBI::dbGetQuery(
        con
        , sql_line
      )

      print("sql_results")
      print(sql_results)

      DBI::dbDisconnect(con)

      return(sql_results)

    })


    #### <<<<   OBSERVES         >>>>  ####
    #-------------------------------------#


    #### <<<<   OBSERVE EVENTS   >>>>  ####
    #-------------------------------------#






    observeEvent(input$btn_confirm_edit_account, {

      # WIP btn_confirm_edit_account ####

      # account_id <- paste0(
      #   stringr::str_sub(input$txt_add_account, 1, 4)
      #   , '_'
      #   , create_unique_ids(1))

      sql_line <- paste0(
        "insert into accounts.accounts (
        account_id
        , account
        , status
        , account_contact_given_name
        , account_contact_surname_name
        , account_contact_email
        , account_contact_cell
        )
        values
        ("
        , paste0("'", edit_account_rctv()$account_id, "',")
        , paste0("'", input$txt_edit_account, "',")
        , paste0("'", input$slt_edit_account_status, "',")
        , paste0("'", input$txt_edit_account_details_name, "',")
        , paste0("'", input$txt_edit_account_details_surname, "',")
        , paste0("'", input$txt_edit_account_details_email, "',")
        , paste0("'", input$txt_edit_account_details_cell, "');")
      )


      # print('sql_line')
      # print(sql_line)

      con <- appbench::database_connection()

      DBI::dbExecute(
        con
        , sql_line
      )

      DBI::dbDisconnect(con)

      # print(sql_line)
      removeModal()

    })


    # should these not be the inputs? not the defaulkes from edit_account_rctv?

    observeEvent(input$btn_commit_edit_account,{


      if(input$txt_edit_account == ''){

        showModal(modalDialog(
          title = "As a minimum please enter an account name!"
          , easyClose = TRUE
          , footer = NULL
          , br()
        ))

      } else {

        showModal(modalDialog(
          title = "You are about to edit the account details!"
          , easyClose = TRUE
          , footer = NULL
          , br()
          , h5(paste0('Account: ', input$txt_edit_account))
          , h5(paste0('Status: ' , input$slt_edit_account_status))
          , br()
          , h5(paste0('Contact details for ', input$txt_edit_account, '...'))
          , br()
          , h5(paste0('Given Name: ', input$txt_edit_account_details_name))
          , h5(paste0('Surame: '    , input$txt_edit_account_details_surname))
          , h5(paste0('Email: '     , input$txt_edit_account_details_email))
          , h5(paste0('Cell: '      , input$txt_edit_account_details_cell))
          , br()
          , br()
          , actionButton(ns('btn_confirm_edit_account'), 'CONFIRM')
          , br()
          , br()
        ))

      }



      removeModal()

    })

    observeEvent(input$btn_fetch_edit_account,{

      req(edit_account_rctv())

      output$outpt_edit_account_left <- renderUI({

        tagList(
          tags$style(type="text/css"  , paste0("#", ns("txt_edit_account {color: white}")))
          , br()
          , textInput(
            ns('txt_edit_account')
            , 'Account Name'
            , value = edit_account_rctv() %>% dplyr::pull(account)
          )
          , br()
          , selectInput(ns('slt_edit_account_status')
                        , 'Status'
                        , choices = c('active', 'inactive')
                        , selected = edit_account_rctv() %>% dplyr::pull(status)
          )
        )

      })


      output$outpt_edit_account_right <- renderUI({

        tagList(
          tags$style(type="text/css"  , paste0("#", ns("txt_edit_account_details_name {color: white}")))
          , tags$style(type="text/css"  , paste0("#", ns("txt_edit_account_details_surname {color: white}")))
          , tags$style(type="text/css"  , paste0("#", ns("txt_edit_account_details_email {color: white}")))
          , tags$style(type="text/css"  , paste0("#", ns("txt_edit_account_details_cell {color: white}")))
          , br()
          , textInput(ns('txt_edit_account_details_name')
                      , 'Given Name'
                      , value = edit_account_rctv() %>% dplyr::pull(account_contact_given_name)
          )
          , br()
          , textInput(ns('txt_edit_account_details_surname')
                      , 'Surname'
                      , value = edit_account_rctv() %>% dplyr::pull(account_contact_surname_name)
          )
          , br()
          , textInput(ns('txt_edit_account_details_email')
                      , 'Email'
                      , value = edit_account_rctv() %>% dplyr::pull(account_contact_email)
          )
          , br()
          , textInput(ns('txt_edit_account_details_cell')
                      , 'Cell No'
                      , value = edit_account_rctv() %>% dplyr::pull(account_contact_cell)
          )

        )

      })



    })


    observeEvent(input$btn_confirm_add_account,{

      # WIP btn_confirm_add_account ####

      account_id <- paste0(
        stringr::str_sub(input$txt_add_account, 1, 4)
        , '_'
        , create_unique_ids(1))


      # sql_line <- paste0(
      #   "insert into accounts.accounts (account_id
      #       , account
      #       , status
      #       , account_contact_given_name
      #       , account_contact_surname_name
      #       , account_contact_email
      #       , account_contact_cell
      #       , time_stamp)
      #   values
      #   ("
      #   , paste0("'", account_id, "',")
      #   , paste0("'", input$txt_add_account, "',")
      #   , paste0("'", input$slt_account_status, "',")
      #   , paste0("'", input$txt_add_account_details_name, "',")
      #   , paste0("'", input$txt_add_account_details_surname, "',")
      #   , paste0("'", input$txt_add_account_details_email, "',")
      #   , paste0("'", input$txt_add_account_details_cell, "',")
      #   , paste0("'", Sys.time(), "');")
      # )

      sql_line <- paste0(
        "insert into accounts.accounts (account_id
            , account
            , status
            , account_contact_given_name
            , account_contact_surname_name
            , account_contact_email
            , account_contact_cell)
        values
        ("
        , paste0("'", account_id, "',")
        , paste0("'", input$txt_add_account, "',")
        , paste0("'", input$slt_account_status, "',")
        , paste0("'", input$txt_add_account_details_name, "',")
        , paste0("'", input$txt_add_account_details_surname, "',")
        , paste0("'", input$txt_add_account_details_email, "',")
        , paste0("'", input$txt_add_account_details_cell, "');")
        # , paste0("'", Sys.time(), "');")
      )

      print('add sql line')
      print(sql_line)


      con <- appbench::database_connection()

      DBI::dbExecute(
        con
        , sql_line
      )

      DBI::dbDisconnect(con)

      gargoyle::trigger("account_slt")
      gargoyle::trigger("updt_edit_account_choices")

      removeModal()

    })


    observeEvent(input$btn_commit_add_account,{


      con <- appbench::database_connection()

      existing_account <- DBI::dbGetQuery(
        con
        , 'select distinct (account) from accounts.accounts'
      ) %>%
        dplyr::pull(account)

      DBI::dbDisconnect(con)

      # check if account exists first
      if(input$txt_add_account %in% existing_account){

        showModal(modalDialog(
          title = "Account already on file!"
          , easyClose = TRUE
          , footer = NULL
          , br()
        ))

      } else if(input$txt_add_account == ''){

        showModal(modalDialog(
          title = "As a minimum please enter an account name!"
          , easyClose = TRUE
          , footer = NULL
          , br()
        ))

      } else {

        showModal(modalDialog(
          title = "You are about to add a new account!"
          , easyClose = TRUE
          , footer = NULL
          , br()
          , h5(paste0('Account: '   , input$txt_add_account))
          , h5(paste0('Status: '    , input$slt_account_status))
          , br()
          , h5(paste0('Contact details for ', input$txt_add_account, '...'))
          , br()
          , h5(paste0('Given Name: ', input$txt_add_account_details_name))
          , h5(paste0('Surname: '   , input$txt_add_account_details_surname))
          , h5(paste0('Email: '     , input$txt_add_account_details_email))
          , h5(paste0('Cell No: '   , input$txt_add_account_details_cell))
          , br()
          , br()
          , actionButton(ns('btn_confirm_add_account'), 'CONFIRM')
          , br()
          , br()
        ))
      }
    })

    observeEvent(input$btn_refresh_add_account,{

      updateTextInput(
        session
        , 'txt_add_account'
        , 'Account Name'
        , value = ''
      )

      updateTextInput(
        session
        , 'txt_add_account_details_name'
        , 'Given Name'
        , value = ''
      )

      updateTextInput(
        session
        , 'txt_add_account_details_surname'
        , 'Surname'
        , value = ''
      )

      updateTextInput(
        session
        , 'txt_add_account_details_email'
        , 'Email'
        , value = ''
      )

      updateTextInput(
        session
        , 'txt_add_account_details_cell'
        , 'Cell No'
        , value = ''
      )

    })


    #### <<<<    OUTPUTS         >>>>  ####
    #-------------------------------------#


    output$outpt_slt_edit_account <- renderUI({

      tagList(
        selectInput(ns('slt_edit_account')
                    , 'Select Account'
                    , choices = edit_account_choices_rctv()
                    , width = '80%'
        )
      )

    })

    output$outpt_accounts_on_file_actv <- DT::renderDataTable({

      # req(input$btn_refresh_accounts)
      req(accounts_on_file_rctv())




      x <- accounts_on_file_rctv() %>%
        dplyr::filter(status == 'active')

      colnames(x) <- paste0('<span style="color:',
                            c("white"),'">',
                            colnames(x),'</span>')

      DT::datatable(
        x
        , callback = DT::JS(callback)
        , filter = 'top'
        , rownames = F
        , escape   = F
        , options = list(scrollX = TRUE)
        , class = 'white-space: nowrap'
      ) %>%
        DT::formatStyle(names(x),
                    backgroundColor = "#bdacac"
        )

    })


    output$outpt_accounts_on_file_inactv <- DT::renderDataTable({

      req(input$btn_refresh_accounts)
      req(accounts_on_file_rctv())

      # refresh
      input$btn_refresh_accounts


      x <- accounts_on_file_rctv() %>%
        dplyr::filter(status == 'inactive')

      colnames(x) <- paste0('<span style="color:',
                            c("white"),'">',
                            colnames(x),'</span>')

      DT::datatable(
        x
        , callback = DT::JS(callback)
        , filter = 'top'
        , rownames = F
        , escape   = F
        , options = list(scrollX = TRUE)
        , class = 'white-space: nowrap'
      ) %>%
        DT::formatStyle(names(x),
                    backgroundColor = "#bdacac"
        )

    })


  })
}


