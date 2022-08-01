#' review_hours UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_review_hours_ui <- function(id){
  ns <- NS(id)
  tagList(
      fluidRow(
        column(4
               , dateRangeInput(
                 ns('dt_rvw_hrs')
                 , 'Select Date Range'
                 , start = lubridate::today() - lubridate::days(5)
                 , end   = lubridate::today()
                 )
               )
        , column(2
                 , actionButton(ns('btn_fetch_hours'), 'Fetch')
                 )
      )
      , fluidRow(
        column(12
               , tableOutput(ns('ot_res_fetch_hours'))
               )
      )
  )
}

#' review_hours Server Functions
#'
#' @noRd
mod_review_hours_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    #### <<<<    CALLMODULES     >>>>  ####
    #-------------------------------------#

    #### <<<<    STATIC VALUES   >>>>  ####
    #-------------------------------------#


    #### <<<<   REACTIVES        >>>>  ####
    #-------------------------------------#


    #### <<<<   REACTIVES VALS   >>>>  ####
    #-------------------------------------#


    #### <<<<   EVENT REACTIVES  >>>>  ####
    #-------------------------------------#

    res_fetch_hours <- eventReactive(input$btn_fetch_hours,{

      con <- appbench::database_connection()

      daily_hours <- con %>% dplyr::tbl(dbplyr::in_schema('services', 'daily_hours'))

      daily_hours <- daily_hours %>%
        dplyr::filter(date >= as.Date(!!input$dt_rvw_hrs[1])) %>%
        dplyr::select(date,  start, end, hours, account) %>%
        dplyr::arrange(date,  start, account) %>%
        dplyr::collect() %>%
        dplyr::mutate(date = strftime(date, format="%Y-%m-%d"))

      DBI::dbDisconnect(con)

      return(daily_hours)

    })


    #### <<<<   OBSERVES         >>>>  ####
    #-------------------------------------#


    #### <<<<   OBSERVE EVENTS   >>>>  ####
    #-------------------------------------#



    #### <<<<    OUTPUTS         >>>>  ####
    #-------------------------------------#

    output$ot_res_fetch_hours <- renderTable({

      res_fetch_hours()

    })


    #### <<<<    OUTPUT OPTIONS  >>>>  ####
    #-------------------------------------#

  })
}
