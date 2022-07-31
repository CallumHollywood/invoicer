#' genr8_day_form UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_genr8_day_form_ui <- function(id){
  ns <- NS(id)
  tagList(
    # fluidRow(
      # column(3
              dateInput(
               ns('dt_entr_day')
                  , 'Select Date'
                  , value = lubridate::today()
             )

      # )
    # )
  )
}

#' genr8_day_form Server Functions
#'
#' @noRd
mod_genr8_day_form_server <- function(
  id
  ){
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


    #### <<<<   OBSERVES         >>>>  ####
    #-------------------------------------#


    #### <<<<   OBSERVE EVENTS   >>>>  ####
    #-------------------------------------#


    #### <<<<    OUTPUTS         >>>>  ####
    #-------------------------------------#

    #### <<<<    OUTPUT OPTIONS  >>>>  ####
    #-------------------------------------#


    #### <<<<    RETURNS         >>>>  ####
    #-------------------------------------#

    return(list(
      dt_entr_day = reactive({input$dt_entr_day})
    ))

  })
}

## To be copied in the UI
# mod_genr8_day_form_ui("genr8_day_form_1")

## To be copied in the server
# mod_genr8_day_form_server("genr8_day_form_1")
