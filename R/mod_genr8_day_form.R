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
    # fluidRow(h5(ns(paste0("hrsfrm_ui_", id))))
    fluidRow(
      column(2
             , dateInput(
               ns('dt_entr_day')
               , 'Select Date'
               , value = lubridate::today()
               , width = '80%'
             )
             , div(
               id = ns('btns')
               , actionButton(ns("btn_gen_r_8_form"), "GENR8 a Record", width = '80%')
               , actionButton(ns("btn_db_commit"), "db_commit", width = '80%')
             )

      )
      , column(6
               , tags$div(id = ns(paste0("hrsfrm_ui_", id)))
      )
      , column(4
               , tableOutput(ns('ot_all_form_sums'))
      )

    )
    , br()
  )
}

#' genr8_day_form Server Functions
#'
#' @noRd
mod_genr8_day_form_server <- function(
  id
  , genr8_dayform
  , dstnct_accounts_in
  , pass_around
){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    #### <<<<    CALLMODULES     >>>>  ####
    #-------------------------------------#

    #### <<<<    STATIC VALUES   >>>>  ####
    #-------------------------------------#

    gen_hrsfrms      <- reactiveValues()
    hrfrm_id    <- 1



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


    observeEvent(input$btn_db_commit,{

      shinyjs::hide('btns')

      slctr <- paste0("#", ns(paste0("hrsfrm_ui_", ns(''))))
      slctr <- substr(slctr, 1, nchar(slctr)-1)

      table_id <- DBI::Id(
        schema  = "services"
        , table   = 'daily_hours'
      )

      con <- appbench::database_connection()

      DBI::dbWriteTable(
        con
        , name      = table_id
        , value     = all_form_values_rctv()
        , row.names = F
        , append    = T
        , overwrite = F
      )

      DBI::dbDisconnect(con)

      shinyalert::shinyalert(
        "Congrats"
        # id
        , "These records were written to the database"
        , type = "success"
      )

      removeUI(
        selector = slctr
      )

    })


    observeEvent(input$btn_gen_r_8_form,{

      x_id <- paste0("dyfrm_", hrfrm_id)

      gen_hrsfrms[[x_id]] <- mod_genr8_hrs_form_server(
        id = x_id
        , dt_entr_day = reactive({input$dt_entr_day})
      )

      dstnct_accounts_thru <- dstnct_accounts_in()

      slctr <- paste0("#", ns(paste0("hrsfrm_ui_", ns(''))))
      slctr <- substr(slctr, 1, nchar(slctr)-1)

      insertUI(
        selector = slctr
        , ui = mod_genr8_hrs_form_ui(
          ns(x_id)
          , dstnct_accounts_in = dstnct_accounts_thru
        )
      )

      hrfrm_id <<- hrfrm_id + 1

    })


    #### <<<<    OUTPUTS         >>>>  ####
    #-------------------------------------#

    all_form_values_rctv <- reactive({

      res <- lapply(reactiveValuesToList(gen_hrsfrms), function(current_module_output) {

        current_module_output$select_values_all()

      })

      # prevent to show an error message when the first module is added
      if (length(res) != 0 && !is.null(res[[1]]$account)) {
        dplyr::bind_rows(res)
      } else {
        NULL
      }

    })

    output$ot_all_form_sums <- renderTable({

      all_form_sums_rctv()

    })

    all_form_sums_rctv <- reactive({

      req(all_form_values_rctv())

      all_form_values_rctv() %>%
        dplyr::select(account, hours) %>%
        dplyr::group_by(account) %>%
        dplyr::summarise(account_hours = sum(hours)) %>%
        dplyr::mutate(account_hours = as.numeric(account_hours)) %>%
        dplyr::arrange(account) %>%
        janitor::adorn_totals()

    })


    #### <<<<    OUTPUT OPTIONS  >>>>  ####
    #-------------------------------------#


    #### <<<<    RETURNS         >>>>  ####
    #-------------------------------------#

    return(list(

    ))

  })
}
