#' cntrl_bar UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_cntrl_bar_ui <- function(id){
  ns <- NS(id)
  tagList(
    column(12)
  )
}

#' cntrl_bar Server Functions
#'
#' @noRd
mod_cntrl_bar_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

