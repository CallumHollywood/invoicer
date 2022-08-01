#' invoice_tab UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_invoice_tab_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      bs4Dash::tabsetPanel(
        id = "tabsetpanel3",
        selected = "invoices",
        vertical = TRUE,
        tabPanel(
          "invoices"
          , bs4Dash::tabBox(
            title = 'invoices'
            , elevation = 2
            , id = ns("tb_inv")
            , width = 12
            , collapsible = FALSE
            , closable = FALSE
            , type = "tabs"
            , status = "primary"
            , solidHeader = TRUE
            , selected = "generate"
            , side = "right"
            , tabPanel(
              "generate"
              , fluidRow(
                column(3
                       , align = 'center'
                       , br()
                       , selectInput(
                         ns('slt_client')
                         , 'Client'
                         , choices = sn_env$sn_clients
                         )
                       # , actionButton(ns('btn_user_rfrsh'), 'Refresh', width = '80%')
                )
                , column(9
                         , align = 'center'
                         , br()
                         , bs4Dash::infoBox(
                           title      = "User Base"
                           , color    = "primary"
                           , gradient = TRUE
                           , fill     = TRUE
                           , width    = 12
                           , icon = shiny::icon("fas fa-chart-bar")
                         )
                )
              )
            )
          )
        )
      )
    )
  )
}

#' invoice_tab Server Functions
#'
#' @noRd
mod_invoice_tab_server <- function(
  id
  , sn_env
  # , dstnct_accounts_rctv
  ){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # observeEvent(dstnct_accounts_rctv(),{
    #
    #   print('dstnct_accounts_rctv()')
    #   print(dstnct_accounts_rctv())
    #
    # })


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

    observeEvent(id,{

      updateSelectInput(
        session
        , 'slt_client'
        , 'Client'
        , choices = sn_env$sn_clients
      )

    })

    #### <<<<   OBSERVE EVENTS   >>>>  ####
    #-------------------------------------#


    #### <<<<    OUTPUTS         >>>>  ####
    #-------------------------------------#

    #### <<<<    OUTPUT OPTIONS  >>>>  ####
    #-------------------------------------#

  })
}
