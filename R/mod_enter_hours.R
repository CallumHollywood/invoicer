#' enter_hours UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList


# con <- appbench::database_connection()
# DBI::dbDisconnect(con)

mod_enter_hours_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(2
             , mod_genr8_day_form_ui(ns("genr8_day_form_1"))
             , actionButton(ns("btn_gen_r_8_form"), "GENR8 a Record")
             , br()
             , actionButton(ns("btn_write_rds"), "write_rds")
      )
      , column(10
             , fluidRow(
               # column(width = 2, actionButton(ns("btn_gen_r_8_form"), "GEN R 8 a FORM"))
               column(width = 6, tags$div(id = ns("add_UI_here")))
               , column(width = 4, tableOutput(ns("all_form_values_table")))
             )
      )
    )
  )
}

#' enter_hours Server Functions
#'
#' @noRd
mod_enter_hours_server <- function(
  id
  , menu_left_main_trigger
){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    #### <<<<    CALLMODULES     >>>>  ####
    #-------------------------------------#

    md_genr8_day_form_server <- mod_genr8_day_form_server(
      "genr8_day_form_1"
    )

    #### <<<<    STATIC VALUES   >>>>  ####
    #-------------------------------------#

    gen_forms  <- reactiveValues()
    current_id <- 1

    #### <<<<   REACTIVES        >>>>  ####
    #-------------------------------------#


    # all_form_sums_rctv <- reactive({
    #
    #   all_form_values_rctv() %>%
    #     dplyr::select()
    #
    # })

    all_form_values_rctv <- reactive({



      res <- lapply(reactiveValuesToList(gen_forms), function(current_module_output) {

        current_module_output$select_values_all() %>%
          dplyr::rename(date = dt_entr_day) %>%
          dplyr::mutate(date = strftime(date, format="%Y-%m-%d")) %>%
          dplyr::mutate(start = glue::glue('{strt_hr}:{strt_qtr}')) %>%
          dplyr::mutate(end = glue::glue('{end_hr}:{end_qtr}')) %>%
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
          dplyr::mutate(hours = difftime(date_end,date_start, units = "hours"))

      })

      # prevent to show an error message when the first module is added
      if (length(res) != 0 && !is.null(res[[1]]$account)) {
        dplyr::bind_rows(res)
      } else {
        NULL
      }

    })


    #### <<<<   REACTIVES VALS   >>>>  ####
    #-------------------------------------#


    #### <<<<   EVENT REACTIVES  >>>>  ####
    #-------------------------------------#


    #### <<<<   OBSERVES         >>>>  ####
    #-------------------------------------#


    #### <<<<   OBSERVE EVENTS   >>>>  ####
    #-------------------------------------#

    observeEvent(input$btn_write_rds,{

      readr::write_rds(all_form_values_rctv(), "dev/rds/all_form_values_rctv.rds")

    })

    menu_left_main_trgr_rctv <- reactiveVal(NULL)

    observeEvent(menu_left_main_trigger(),{

      menu_left_main_trgr_rctv(menu_left_main_trigger())

    })


    dstnct_accounts <- eventReactive(input$btn_gen_r_8_form,{

      db_sql <- 'select distinct account from accounts.accounts order by account;'

      message(db_sql)

      con <- appbench::database_connection()

      rtrnr <- DBI::dbGetQuery(
        con
        , db_sql
      ) %>%
        dplyr::pull(account)

      DBI::dbDisconnect(con)

      return(rtrnr)

    })


    observeEvent(dstnct_accounts(),{

      print('dstnct_accounts')
      print(dstnct_accounts())

    })

    observeEvent(input$btn_gen_r_8_form, {



      x_id <- paste0("module_", current_id)

      # gen_forms[[x_id]] <- gen_r_8_form(id = x_id)

      gen_forms[[x_id]] <- mod_genr8_hrs_form_server(
        id = x_id
        , dstnct_accounts_in = dstnct_accounts
        , trigger = reactive({menu_left_main_trgr_rctv()})
        , dt_entr_day = md_genr8_day_form_server$dt_entr_day
      )

      insertUI(selector = paste0("#", ns("add_UI_here")),
               ui = mod_genr8_hrs_form_ui(ns(x_id)))
      # ui = mod_genr8_hrs_form_ui(x_id))

      print('gen_forms')
      print(gen_forms)

      current_id <<- current_id + 1

      output$all_form_values_table <- renderTable({

        req(md_genr8_day_form_server$dt_entr_day)

        all_form_values_rctv() %>%
          # dplyr::rename(date = dt_entr_day) %>%
          # dplyr::mutate(date = strftime(date, format="%Y-%m-%d")) %>%
          # dplyr::mutate(date_start = strftime(date_start
          #                                     , format="%Y-%m-%d %H:%M"
          #                                     , tz = "GMT"
          #                                     )) %>%
          # dplyr::mutate(start = glue::glue('{strt_hr}:{strt_qtr}')) %>%
          # dplyr::mutate(end = glue::glue('{end_hr}:{end_qtr}')) %>%
          dplyr::select(date, account, date_start, date_end, hours
                        # , start, end
                        )
      })

    })



    #### <<<<    OUTPUTS         >>>>  ####
    #-------------------------------------#

    # output$all_form_values_table <- renderTable({
    #
    #   req(md_genr8_day_form_server$dt_entr_day)
    #
    #   all_form_values_rctv() %>%
    #     dplyr::rename(date = dt_entr_day) %>%
    #     dplyr::mutate(date = strftime(date, format="%Y-%m-%d"))
    # })


    #### <<<<    OUTPUT OPTIONS  >>>>  ####
    #-------------------------------------#




  })
}

## To be copied in the UI
# mod_enter_hours_ui("enter_hours_1")

## To be copied in the server
# mod_enter_hours_server("enter_hours_1")
