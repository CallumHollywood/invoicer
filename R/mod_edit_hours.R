#' edit_hours UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_edit_hours_ui <- function(id){
  ns <- NS(id)
  tagList(
 'mod_edit_hours_ui'
  )
}

#' edit_hours Server Functions
#'
#' @noRd
mod_edit_hours_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_edit_hours_ui("edit_hours_1")

## To be copied in the server
# mod_edit_hours_server("edit_hours_1")
