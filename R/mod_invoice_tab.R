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
        id = ns("tb_inv"),
        # id = "tb_inv",
        selected = "invoices",
        vertical = TRUE,
        tabPanel(
          "invoices"
          , bs4Dash::tabBox(
            title = 'INVOICES'
            , elevation = 2
            , id = ns("tb_inv2")
            # , id = "tb_inv2"
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
                         , width = '80%'
                         )
                       , actionButton(ns('btn_ftch_inv_data'), 'fetch'
                                      , width = '80%'
                                      )
                       , actionButton(ns('btn_prvw_inv_data'), 'preview'
                                      , width = '80%'
                       )

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
                           # , icon = shiny::icon("cog", verify_fa = FALSE)
                           , icon = shiny::icon('ice-lolly-tasted', lib="glyphicon")
                           , br()
                           , tableOutput(ns('ot_invc_client'))
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

    # invc_client_rctv <- reactiveVal(NULL)


    # observeEvent(input$btn_ftch_inv_data, {
    #
    #   message('input$btn_ftch_inv_data')
    #   message(input$btn_ftch_inv_data)
    #
    # })

    # observeEvent(input$btn_ftch_inv_data, {
    #
    #   sql_ftch_inv_data <- glue::glue(
    #     "select * from services.daily_hours where account = '{input$slt_client}';"
    #   )
    #
    #   message(sql_ftch_inv_data)
    #
    #   con <- appbench::database_connection()
    #
    #   res <- DBI::dbGetQuery(
    #     con
    #     , sql_ftch_inv_data
    #   )
    #
    #   DBI::dbDisconnect(con)
    #
    #   invc_client_rctv(res)
    #
    #   # return(res)
    #
    # })

    invc_client_rctv <- eventReactive(input$btn_ftch_inv_data, {

      sql_ftch_inv_data <- glue::glue(
        "select * from services.daily_hours where account = '{input$slt_client}';"
      )

      message(sql_ftch_inv_data)

      con <- appbench::database_connection()

      res <- DBI::dbGetQuery(
        con
        , sql_ftch_inv_data
      )

      DBI::dbDisconnect(con)

      # invc_client_rctv(res)

      return(res)

    })


    #### <<<<   OBSERVES         >>>>  ####
    #-------------------------------------#


    #### <<<<   OBSERVE EVENTS   >>>>  ####
    #-------------------------------------#

    observeEvent(input$btn_prvw_inv_data,{

      showModal(modalDialog(
        title       = 'Invoice PREVIEW'
        , size      = 'l'
        , easyClose = T
        , h6("PUT 3 reports here")
        , withTags(
          tags$ol(
            tags$li("hours")
            , tags$li("flat charges")
            , tags$li("total")
          )
        )
        ,
      ))

    })


    # CONFIRM if this IS required - same init values on UI
    observeEvent(id,{

      updateSelectInput(
        session
        , 'slt_client'
        , 'Client'
        , choices = sn_env$sn_clients
      )

    })


    #### <<<<    OUTPUTS         >>>>  ####
    #-------------------------------------#

    output$ot_invc_client <- renderTable({

      invc_client_rctv() %>%
        dplyr::select(date, start, end_time, hours)

    })

    #### <<<<    OUTPUT OPTIONS  >>>>  ####
    #-------------------------------------#

  })
}
