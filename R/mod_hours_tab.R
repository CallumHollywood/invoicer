#' hours_tab UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_hours_tab_ui <- function(id){
  ns <- NS(id)
  tagList(
    bs4Dash::tabBox(
      title         = "HOURS"
      , elevation   = 2
      , id          = ns("ntrhrs")
      , width       = 12
      , collapsible = FALSE
      , closable    = FALSE
      , type        = "tabs"
      , status      = "primary"
      , solidHeader = TRUE
      , selected    = "enter"
      , side        = "right"
      , tabPanel(
        "enter"
        , mod_enter_hours_ui(ns("enter_hours_1"))
      )
      , tabPanel(
        "review"
        , mod_review_hours_ui(ns("review_hours_1"))
      )
      # , tabPanel(
      #   "edit"
      #   , mod_edit_hours_ui("edit_hours_1")
      # )


    )
  )
}

#' hours_tab Server Functions
#'
#' @noRd
mod_hours_tab_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    #### <<<<    gargoyle        >>>>  ####
    #-------------------------------------#

    #### <<<<    CALLMODULES     >>>>  ####
    #-------------------------------------#

    mod_enter_hours_server(
      "enter_hours_1"
      , pass_around
    )

    mod_review_hours_server(
      "review_hours_1"
    )

    mod_edit_hours_server(
      "edit_hours_1"
    )

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
  })
}
## To be copied in the UI
# mod_hours_tab_ui("hours_tab_1")

## To be copied in the server
# mod_hours_tab_server("hours_tab_1")
