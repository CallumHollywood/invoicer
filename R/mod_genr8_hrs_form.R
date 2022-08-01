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
      column(4
             , style = 'background-color: #707987;'
             , fluidRow(
               column(3
                      , checkboxInput(ns('chbx_incl_hrs')
                                      , 'Include'
                                      , value = T
                                      )
                      )
               , column(9

                        , uiOutput(ns('ot_slt_account'))

               )
             )
      )
      , column(4
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
      , column(4
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
  # , trigger
  , dt_entr_day
){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # observe( {
    #
    #   my_choices <- isolate(dstnct_accounts_in())
    #
    #   print('my_choices')
    #   print(my_choices)
    #
    #   message(id)
    #
    #
    #
    #   updateSelectInput(
    #     session,
    #     'slt_account'
    #     , 'slt_account'
    #     , choices = my_choices
    #   )
    #
    # })


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

      req(dt_entr_day())
      req(input$slt_strt_hr)
      req(input$slt_strt_qtr)
      req(input$slt_end_hr)
      req(input$slt_end_qtr)
      req(input$slt_account)

      # tibble::tibble(
      #   forename  = 'bob'# input$slt_forename
      #   , surname  = 'costa' #input$slt_surname
      #   )
      # tibble::tibble(x=1)
      tibble::tibble(
        id = as.numeric(stringr::str_remove(id, 'module_' ))
        ,
        dt_entr_day = isolate(dt_entr_day())
        ,
        account  = input$slt_account
        , strt_hr  = input$slt_strt_hr
        , strt_qtr = input$slt_strt_qtr
        , end_hr   = input$slt_end_hr
        , end_qtr  = input$slt_end_qtr
        , incl_hrs = input$chbx_incl_hrs
      ) %>%

        dplyr::rename(date = dt_entr_day) %>%
        dplyr::mutate(date = strftime(date, format="%Y-%m-%d")) %>%
        # dplyr::mutate(start = glue::glue('{strt_hr}:{strt_qtr}')) %>%
        dplyr::mutate(start = paste0(strt_hr, ':', strt_qtr)) %>%
        # dplyr::mutate(end = glue::glue('{end_hr}:{end_qtr}')) %>%
        dplyr::mutate(end = paste0(end_hr, ':', end_qtr)) %>%
        dplyr::mutate(date_start = lubridate::ymd_hm(
          paste0(date, " ", start)
        )) %>%
        dplyr::mutate(date_start = strftime(date_start
                                            , format="%Y-%m-%d %H:%M"
                                            , tz = "GMT"
        )) %>%

        dplyr::mutate(date_end = lubridate::ymd_hm(
          paste0(date, " ", end)
        )) %>%
        dplyr::mutate(date_end = strftime(date_end
                                          , format="%Y-%m-%d %H:%M"
                                          , tz = "GMT"
        )) %>%
        dplyr::mutate(hours = difftime(date_end,date_start, units = "hours")) %>%
        dplyr::filter(incl_hrs == T)


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
                    , choices = isolate(dstnct_accounts_in())
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
