#' genr8_hrs_form UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList

mod_genr8_hrs_form_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(2
             # , column(12
                      , style = 'background-color: #707987;'
                      , uiOutput(ns('ot_slt_account'))
             # )
      )
      , column(5
               , style = 'background-color: #c3d0e6;'
               , fluidRow(
                 column(6
                        , selectInput(ns('slt_strt_hr')
                                      , 'Hour'
                                      , choices  = c(paste0('0', 0:9), 10:23)
                                      , selected = '08'
                        )

                 )
                 , column(6
                          , selectInput(ns('slt_strt_qtr')
                                        , 'Qtr'
                                        , choices  = c('00','15','30','45')
                                        , selected = '00'
                          )
                 )
               )
      )
      , column(5
               , style = 'background-color: #707987;'
               , fluidRow(
                 column(6
                        , selectInput(ns('slt_end_hr')
                                      , 'Hour'
                                      , choices  = c(paste0('0', 0:9), 10:23)
                                      , selected = '13'
                        )

                 )
                 , column(6
                          , selectInput(ns('slt_end_qtr')
                                        , 'Qtr'
                                        , choices  = c('00','15','30','45')
                                        , selected = '00'
                          )
                 )
               )
      )

    )
  )
}

#' genr8_hrs_form Server Functions
#'
#' @noRd
mod_genr8_hrs_form_server <- function(
  id
  , dstnct_accounts_in
  , trigger
  , dt_entr_day
){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    #### <<<<    CALLMODULES     >>>>  ####
    #-------------------------------------#

    #### <<<<    STATIC VALUES   >>>>  ####
    #-------------------------------------#


    #### <<<<   REACTIVES        >>>>  ####
    #-------------------------------------#

    # dstnct_accounts <- reactive({
    #
    #   db_sql <- 'select distinct account from accounts.accounts order by account'
    #
    #   con <- appbench::database_connection()
    #
    #   rtrnr <- DBI::dbGetQuery(
    #     con
    #     , db_sql
    #   ) %>%
    #     dplyr::pull(account)
    #
    #   DBI::dbDisconnect(con)
    #
    #   return(rtrnr)
    #
    # })

    select_values_all <- reactive({

      req(input$slt_account)

      # tibble::tibble(
      #   forename  = 'bob'# input$slt_forename
      #   , surname  = 'costa' #input$slt_surname
      #   )
      # tibble::tibble(x=1)
      tibble::tibble(
        dt_entr_day = dt_entr_day()
        , account  = input$slt_account
        , strt_hr  = input$slt_strt_hr
        , strt_qtr = input$slt_strt_qtr
        , end_hr   = input$slt_end_hr
        , end_qtr  = input$slt_end_qtr
      )


    })


    #### <<<<   REACTIVES VALS   >>>>  ####
    #-------------------------------------#


    #### <<<<   EVENT REACTIVES  >>>>  ####
    #-------------------------------------#


    #### <<<<   OBSERVES         >>>>  ####
    #-------------------------------------#

    # observeEvent(trigger(),{
    #
    #   print('--------')
    #   print(trigger())
    #   print(dstnct_accounts_in())
    #
    #   updateSelectInput(
    #     session
    #     , 'slt_account'
    #     , 'account'
    #     , choices = dstnct_accounts_in()
    #   )
    #
    # })


    #### <<<<   OBSERVE EVENTS   >>>>  ####
    #-------------------------------------#


    #### <<<<    OUTPUTS         >>>>  ####
    #-------------------------------------#

    output$ot_slt_account <- renderUI({

      tagList(
        selectInput(ns("slt_account")
                    , 'account'
                    , choices = dstnct_accounts_in()
        )

      )

    })

    #### <<<<    OUTPUT OPTIONS  >>>>  ####
    #-------------------------------------#


    #### <<<<    RETURNS         >>>>  ####
    #-------------------------------------#

    return(list(
      select_values_all = reactive({select_values_all()})
    ))

  })
}

## To be copied in the UI
# mod_genr8_hrs_form_ui("genr8_hrs_form_1")

## To be copied in the server
# mod_genr8_hrs_form_server("genr8_hrs_form_1")
