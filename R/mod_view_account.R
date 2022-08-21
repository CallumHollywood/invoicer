#' view_account UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @importFrom magrittr %>%
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_view_account_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(3
             , uiOutput(ns('outpt_slt_view_account'))
             , selectInput(
               ns('slt_account_dates')
               , 'Select date'
               , choices = NULL
             )
             , br()
             , actionButton(ns('btn_fetch_view_account'), 'Fetch')
      )
      , column(9
               , align = 'left'
               , bs4Dash::infoBox(
                 title      = 'Account'
                 , color    = 'primary'
                 , gradient = T
                 , fill     = T
                 , width    = 12
                 # , icon = shiny::icon("cog", verify_fa = FALSE)
                 , icon = shiny::icon('ice-lolly-tasted', lib="glyphicon")
                 , uiOutput(ns('outpt_view_account'))
                 # , DT::dataTableOutput(ns('outpt_view_account'))
               )
      )
    )

  )
}

#' view_account Server Functions
#'
#' @noRd
mod_view_account_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    #### <<<<    CALLMODULES     >>>>  ####
    #-------------------------------------#

    #### <<<<    STATIC VALUES   >>>>  ####
    #-------------------------------------#


    #### <<<<   REACTIVES        >>>>  ####
    #-------------------------------------#



    account_choices_rctv <- reactive({

      # input$btn_commit_edit_account

      gargoyle::watch("account_slt")

      con <- appbench::database_connection()

      sql_result <- DBI::dbGetQuery(
        con
        , 'select distinct (account) from accounts.accounts order by account;'
      )

      sql_result <- dplyr::pull(sql_result, account)

      DBI::dbDisconnect(con)

      return(sql_result)

    })



    #### <<<<   REACTIVES VALS   >>>>  ####
    #-------------------------------------#


    #### <<<<   EVENT REACTIVES  >>>>  ####
    #-------------------------------------#

    view_account_rctv <- eventReactive(input$btn_fetch_view_account,{

      sql_line <- paste0(
        "select * from accounts.accounts where account = '"
        , input$slt_view_account
        , "';"
      )


      # print('sql_line')
      # print(sql_line)

      con <- appbench::database_connection()

      sql_results <- DBI::dbGetQuery(
        con
        , sql_line
      ) %>%
        dplyr::mutate(time_stamp = stringr::str_sub(as.character(time_stamp), 1, 19)) %>%
        dplyr::filter(time_stamp == input$slt_account_dates)

      DBI::dbDisconnect(con)

      return(sql_results)

    })



    #### <<<<   OBSERVES         >>>>  ####
    #-------------------------------------#


    #### <<<<   OBSERVE EVENTS   >>>>  ####
    #-------------------------------------#

    observeEvent(input$slt_view_account,{

      # print('input$slt_view_account')

      account_dates_choices_sql <- paste0(
        "select time_stamp from accounts.accounts where account = '"
        , input$slt_view_account
        , "';"
      )

      # print(account_dates_choices_sql)

      con <- appbench::database_connection()

      account_dates_choices <- DBI::dbGetQuery(
        con
        , account_dates_choices_sql
      )

      account_dates_choices <- dplyr::pull(account_dates_choices, time_stamp)

      updateSelectInput(
        session
        , 'slt_account_dates'
        , 'Select Date'
        , choices = account_dates_choices

      )

      DBI::dbDisconnect(con)
    })



    #### <<<<    OUTPUTS         >>>>  ####
    #-------------------------------------#



    output$outpt_view_account <- renderUI({

      req(input$slt_account_dates)
      req(view_account_rctv())

        tagList(
          h5('Account ID: ', view_account_rctv()$account_id)
          , h5('status: ', view_account_rctv()$status)
          , h5('Account Contact Given Name: ', view_account_rctv()$account_contact_given_name)
          , h5('Account Contact Surname: ', view_account_rctv()$account_contact_surname_name)
          , h5('Account Contact Email: ', view_account_rctv()$account_contact_email)
          , h5('Account Contact Cell: ', view_account_rctv()$account_contact_cell)
        )

    })



    output$outpt_slt_view_account <- renderUI({

      selectInput(
        ns('slt_view_account')
        , 'Select Account'
        , choices =  account_choices_rctv()
      )

    })

  })
}

