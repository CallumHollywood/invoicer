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
    tags$head(HTML('a {color: #fff;}'))
    , fluidRow(
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
            , selected = "raise"
            , side = "right"
            , tabPanel(
              "raise"
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
                       , div(class = "whiteme"
                             , dateInput(ns('dt_inv_frm')
                                         , 'from'
                                         , value = lubridate::today() - months(2)
                             )
                             , dateInput(ns('dt_inv_to')
                                         , 'to'
                                         , value = lubridate::today()
                             )
                       )
                       , br()
                       , actionButton(ns('btn_ftch_inv_data'), 'fetch'
                                      , width = '80%'
                       )
                       , br()
                       , actionButton(ns('btn_prvw_inv_data'), 'raise'
                                      , width = '80%'
                       )

                )
                , column(9
                         , align = 'center'
                         , br()
                         , DT::dataTableOutput(ns('ot_invc_client'))
                         # , bs4Dash::infoBox(
                         #   title      = "Outstanding"
                         #   , color    = "primary"
                         #   , gradient = TRUE
                         #   , fill     = TRUE
                         #   , width    = 12
                         #   # , icon = shiny::icon("cog", verify_fa = FALSE)
                         #   , icon = shiny::icon('ice-lolly-tasted', lib="glyphicon")
                         #   , br()
                         #   , DT::dataTableOutput(ns('ot_invc_client'))
                         # )
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

    invoice_table_rctv <- reactive({

      # invoice_table_thru <- invc_client_rctv() %>%
      #   dplyr::select(role, hours, rate) %>%
      #   dplyr::mutate(shift_ttl = hours * rate) %>%
      #   dplyr::group_by(role) %>%
      #   dplyr::mutate(
      #     ttl_hours = sum(hours, na.rm = T)
      #     , role_ttl = sum(shift_ttl)
      #   ) %>%
      #   dplyr::ungroup() %>%
      #   dplyr::arrange(role) %>%
      #   dplyr::select(role, ttl_hours, rate,  role_ttl) %>%
      #   dplyr::distinct()

      invc_client_rctv() %>%
        dplyr::select(role, hours, rate, currency) %>%
        dplyr::mutate(shift_ttl = hours * rate) %>%
        dplyr::group_by(role, currency, rate) %>%
        dplyr::mutate(
          ttl_hours = sum(hours, na.rm = T)
          , sub_total = sum(shift_ttl)
        ) %>%
        dplyr::ungroup() %>%
        dplyr::arrange(role) %>%
        dplyr::select(role, ttl_hours, rate,  currency , sub_total) %>%
        dplyr::distinct()

      # readr::write_rds(invoice_table_thru, 'inst/app/rds/invoice_table_thru.rds')

      # return(invoice_table_thru)

    })



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

      input_slt_client <- input$slt_client
      input_dt_inv_frm <- input$dt_inv_frm
      input_dt_inv_to  <- input$dt_inv_to

      sql_ftch_inv_data <- glue::glue("select
                d.id
                , d.date
                , d.account
                , d.strt_hr
                , d.strt_qtr
                , d.end_hr
                , d.end_qtr
                , d.incl_hrs
                , d.notes
                , d.start
                , d.end_time
                , d.date_start
                , d.date_end
                , d.hours
                , d.pk_id
                , d.invoiced
                , r.role_id
                , r.role
                , r.rate
                , r.currency
        from services.daily_hours d
        inner join services.role r
        on d.role_id = r.role_id
        where account = '{input_slt_client}'
        and date >= '{input_dt_inv_frm}'
        and date <= '{input_dt_inv_to}'
        and invoiced = FALSE;")



      message(sql_ftch_inv_data)

      con <- appbench::database_connection()

      res <- DBI::dbGetQuery(
        con
        , sql_ftch_inv_data
      )

      readr::write_rds(res, 'res.rds')

      DBI::dbDisconnect(con)

      # invc_client_rctv(res)

      return(res)

    })



    #### <<<<   OBSERVES         >>>>  ####
    #-------------------------------------#

    observe({

      # OBS ot_prnt_inv ####

      message('ot_prnt_inv 1')

      output$ot_prnt_inv <- downloadHandler(

        filename = "Invoice.pdf",
        content = function(file) {

          readr::write_rds(invc_client_rctv(), 'inst/app/rds/invc_client_rctv.rds')

          tempReport <- file.path(tempdir(), "invoice_tmplt_20220912_2.Rmd")
          file.copy("inst/app/mrkdn/invoice_tmplt_20220912_2.Rmd", tempReport, overwrite = TRUE)


          inv_config <- readr::read_csv('inst/app/cnfg/inv_config.csv')

          inv_config           <- as.data.frame(inv_config)
          rownames(inv_config) <- inv_config$param
          inv_config           <- inv_config[names(inv_config) != 'param']
          inv_config           <- as.data.frame(t(inv_config), stringsAsFactors = F)


          # readr::write_rds(invoice_table_rctv(), 'inst/app/rds/invoice_table_rctv.rds')


          params <- list(
            # invc_client = tibble::tibble(x = 1:4)
            invc_client        = invc_client_rctv()
            , invc_table       = invoice_table_rctv()
            , due_date         = inv_config$due_date
            , invoice_no       = inv_config$invoice_no
            , client_in        = inv_config$client_in
            , company_mine     = inv_config$company_mine
            , address_mine_1   = inv_config$address_mine_1
            , address_mine_2   = inv_config$address_mine_2
            , address_mine_3   = inv_config$address_mine_3
            , address_mine_4   = inv_config$address_mine_4
            , company_client   = inv_config$company_client
            , address_client_1 = inv_config$address_client_1
            , address_client_2 = inv_config$address_client_2
            , address_client_3 = inv_config$address_client_3
            , address_client_4 = inv_config$address_client_4
            , account_holder   = inv_config$account_holder
            , account_number   = inv_config$account_number
            , account_bank     = inv_config$account_bank
            , contact          = inv_config$contact
            , email            = inv_config$email
            , phone            = inv_config$phone
          )


          inputEnv <- .GlobalEnv

          message('ot_prnt_inv 3')

          out <- rmarkdown::render(
            tempReport
            , output_file = file
            , params      = params
            , envir       = inputEnv
          )
        }
      )

    })



    #### <<<<   OBSERVE EVENTS   >>>>  ####
    #-------------------------------------#

    # observeEvent(invc_client_rctv(),{
    #
    #   readr::write_rds(invc_client_rctv(), 'dev/rds/invc_client_rctv.rds')
    #
    # })


    observeEvent(input$btn_prvw_inv_data,{

      readr::write_rds(file = 'invc_client_rctv.rds', x = invc_client_rctv())

      invc_client_hrs <- invc_client_rctv() %>%
        dplyr::select(hours) %>%
        colSums()

      names(invc_client_hrs) <- NULL

      showModal(modalDialog(
        title       = 'Invoice PREVIEW + DEV CLIENT NAME'
        , size      = 'l'
        , easyClose = T
        , footer = tagList(
          downloadLink(ns('ot_prnt_inv'), 'Print Invoice')
          , modalButton("Dismiss")
        )
        , h6("DEV - Date Range here")
        , hr()
        , h6("Summary")
        , withTags(
          tags$ol(
            tags$li(glue::glue("hours: ", invc_client_hrs))
            , tags$li("flat charges")
            , tags$li("total")
          )
        )
        , h6('1 Billable Hours')
        , tableOutput(ns('ot_invoice_table_1'))
        , hr()
        , h6('2 Flat Charges')
        , tableOutput(ns('ot_invoice_table_2'))
        , h6('3 Totals and Subtotals')
        , tableOutput(ns('ot_invoice_table_3'))

      )
      )

    })



    output$ot_invoice_table_1 <- renderTable({

      invoice_table_rctv()

    })

    output$ot_invoice_table_2 <- renderTable({

      # TO BE DEVED

    })

    output$ot_invoice_table_3 <- renderTable({

      # TO BE DEVED

    })



    # observeEvent(invoice_table_rctv(),{
    #
    #   readr::write_rds(invoice_table_rctv(), 'dev/rds/invoice_table_rctv.rds')
    #
    # })


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

    output$ot_invc_client <- DT::renderDataTable({

      df_x <- invc_client_rctv() %>%
        dplyr::select(date, role, start, end_time, hours) %>%
        dplyr::mutate(date = stringr::str_sub(date, 1, 10))

      DT::datatable(
        df_x
        , rownames = F
        , options = list(
          dom = 't'
          , initComplete = DT::JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#49688e', 'color': 'white'});",
            "}")
          , columnDefs = list(list(className = 'dt-center', targets = "_all"))
        )
      )


    })

    #### <<<<    OUTPUT OPTIONS  >>>>  ####
    #-------------------------------------#

  })
}
