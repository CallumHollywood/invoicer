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
             , div(class = 'login-ui'
                   , dateRangeInput(
                     ns('dt_rvw_hrs')
                     , 'Select Date Range'
                     , start = lubridate::today() - lubridate::days(5)
                     , end   = lubridate::today()
                   )
             )
      )
      , column(2
               , actionButton(ns('btn_fetch_hours'), 'Fetch')
      )
    )
    , fluidRow(
      column(12
             # , tableOutput(ns('ot_res_fetch_hours'))
             , DT::dataTableOutput(ns('ot_res_fetch_hours'))
      )
      , tags$script(paste0("$(document).on('click', '#", ns('ot_res_fetch_hours')," button', function () {

                                      // alert(this.id);

                                      Shiny.onInputChange('", ns('edit_lastClickId'), "',this.id);
                                      Shiny.onInputChange('", ns('edit_lastClick'), "', Math.random())

                           });"
      )
      )

    )
  )
}

#' review_hours Server Functions
#'
#' @noRd
mod_review_hours_server <- function(
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

    all_acounts <- reactive({

      con <- appbench::database_connection()

      sql_line <- 'select distinct (account) from accounts.accounts order by account'

      # print(sql_line)

      res <- DBI::dbGetQuery(
        con
        , sql_line
      ) %>%
        dplyr::pull(account)

      DBI::dbDisconnect(con)

      return(res)

    })


    edit_values_all <- reactive({

      req(input$dt_edit_date)
      req(input$slt_edit_strt_hr)
      req(input$slt_edit_strt_qtr)
      req(input$slt_edit_end_hr)
      req(input$slt_edit_end_qtr)
      req(input$slt_edit_account)


      dt_edit_date      <- input$dt_edit_date
      slt_edit_strt_hr  <- input$slt_edit_strt_hr
      slt_edit_strt_qtr <- input$slt_edit_strt_qtr
      slt_edit_end_hr   <- input$slt_edit_end_hr
      slt_edit_end_qtr  <- input$slt_edit_end_qtr
      slt_edit_account  <- input$slt_edit_account

      tibble::tibble(
        # id = as.numeric(stringr::str_remove(id, 'dyfrm_' ))
        # ,
        date = dt_edit_date
        , account  = slt_edit_account
        , strt_hr  = slt_edit_strt_hr
        , strt_qtr = slt_edit_strt_qtr
        , end_hr   = slt_edit_end_hr
        , end_qtr  = slt_edit_end_qtr
        # , incl_hrs = input$chbx_incl_hrs
        # , notes    = input$txt_notes
      ) %>%

        # dplyr::rename(date = dt_entr_day) %>%
        dplyr::mutate(date = strftime(date, format="%Y-%m-%d")) %>%
        dplyr::mutate(start = paste0(strt_hr, ':', strt_qtr)) %>%
        dplyr::mutate(end_time = paste0(end_hr, ':', end_qtr)) %>%
        dplyr::mutate(date_start = lubridate::ymd_hm(
          paste0(date, " ", start)
        )) %>%
        dplyr::mutate(date_start = strftime(date_start
                                            , format="%Y-%m-%d %H:%M"
                                            , tz = "GMT"
        )) %>%

        dplyr::mutate(date_end = lubridate::ymd_hm(
          paste0(date, " ", end_time)
        )) %>%
        dplyr::mutate(date_end = strftime(date_end
                                          , format="%Y-%m-%d %H:%M"
                                          , tz = "GMT"
        )) %>%
        dplyr::mutate(hours = as.numeric(difftime(date_end,date_start, units = "hours")))
      # %>%
      #   dplyr::filter(incl_hrs == T)


    })




    #### <<<<   REACTIVES VALS   >>>>  ####
    #-------------------------------------#


    #### <<<<   EVENT REACTIVES  >>>>  ####
    #-------------------------------------#

    edit_fks <- eventReactive(input$edit_lastClick,{

      x <- res_fetch_hours() %>%
        dplyr::filter(pk_id == as.numeric(input$edit_lastClickId))


    })



    res_fetch_hours <- eventReactive(input$btn_fetch_hours,{

      con <- appbench::database_connection()

      daily_hours <- con %>% dplyr::tbl(dbplyr::in_schema('services', 'daily_hours'))


      # CONFIRM the DATE to use!!! date_start?

      daily_hours <- daily_hours %>%
        dplyr::filter(date >= as.Date(!!input$dt_rvw_hrs[1])) %>%
        # ADD < DATE HERE ALSO? NO?
        # dplyr::select(pk_id, date,  start, end, hours, account, notes) %>%
        dplyr::arrange(date,  start, account) %>%
        dplyr::collect() %>%
        dplyr::mutate(date = strftime(date, format="%Y-%m-%d"))

      DBI::dbDisconnect(con)

      print('daily_hours')
      print(daily_hours)


      return(daily_hours)

    })


    #### <<<<   OBSERVES         >>>>  ####
    #-------------------------------------#


    #### <<<<   OBSERVE EVENTS   >>>>  ####
    #-------------------------------------#

    observeEvent(input$btn_updt_db_edit_daily_hours,{

      # delete pk_id row
      # insert pk_id row

      input_slt_edit_account   <- input$slt_edit_account
      input_dt_edit_date       <- input$dt_edit_date
      input_slt_edit_strt_hr   <- input$slt_edit_strt_hr
      input_slt_edit_strt_qtr  <- input$slt_edit_strt_qtr
      input_chbx_edit_incl_hrs <- input$chbx_edit_incl_hrs
      input_slt_edit_end_hr    <- input$slt_edit_end_hr
      input_slt_edit_end_qtr   <- input$slt_edit_end_qtr
      input_txt_edit_notes     <- input$txt_edit_notes

      start_in <- paste0(input_slt_edit_strt_hr, ':', input_slt_edit_strt_qtr)
      end_time   <- paste0(input_slt_edit_end_hr , ':', input_slt_edit_end_qtr)



      updt_sql <- glue::glue(
        "update services.daily_hours
          set
          account  = '{input_slt_edit_account}'
        , date     = '{input_dt_edit_date}'
        , strt_hr  = '{input_slt_edit_strt_hr}'
        , strt_qtr = '{input_slt_edit_strt_qtr}'
        , incl_hrs = {input_chbx_edit_incl_hrs}
        , end_hr   = '{input_slt_edit_end_hr}'
        , end_qtr  = '{input_slt_edit_end_qtr}'
        , notes    = '{input_txt_edit_notes}'
        , hours    = {edit_values_all()$hours}
        , start    = '{start_in}'
        , end_time      = '{end_time}'
        where
        pk_id = {as.numeric(edit_fks()$pk_id)};"
      )
        # , end      = '{end_in}'

      print(updt_sql)

      con <- appbench::database_connection()

      DBI::dbExecute(
        con
        , updt_sql
      )

      DBI::dbDisconnect(con)

      removeModal()

    })


    observeEvent(edit_values_all(),{

      print(edit_values_all())

    })



    observeEvent(edit_fks(),{

      print(edit_fks())

      showModal(modalDialog(
        title = "Edit Record"
        , easyClose = TRUE
        , footer = NULL
        , br()
        , fluidRow(
          column(6
                 , 'Account'
          )
          , column(6
                   , selectInput(ns('slt_edit_account')
                                 , label = NULL
                                 , choices  = all_acounts()
                                 , selected = edit_fks()$account
                   )
          )

        )
        , fluidRow(
          column(6
                 , 'Date'
          )
          , column(6
                   , div(class = 'whiteme'
                   , dateInput(
                     ns('dt_edit_date')
                     , label = NULL
                     , value = edit_fks()$date
                     )
                   )
          )

        )
        , fluidRow(
          column(3
                 , 'Start Time'
          )
          , column(3
                   , selectInput(ns('slt_edit_strt_hr')
                                 , 'Hour'
                                 , choices  = c(paste0('0', 0:9), 10:23)
                                 , selected = edit_fks()$strt_hr
                   )

          )
          , column(3
                   , selectInput(ns('slt_edit_strt_qtr')
                                 , 'Qtr'
                                 , choices  = c('00','15','30','45')
                                 , selected = edit_fks()$strt_qtr
                   )
          )
          , column(3
                   , checkboxInput(ns('chbx_edit_incl_hrs')
                                   , 'Include'
                                   , value = T # edit_fks()$incl_hrs
                   )
          )


        )

        , fluidRow(
          column(3
                 , 'End Time'
          )
          , column(3
                   , selectInput(ns('slt_edit_end_hr')
                                 , 'Hour'
                                 , choices  = c(paste0('0', 0:9), 10:23)
                                 , selected = edit_fks()$end_hr
                   )

          )
          , column(3
                   , selectInput(ns('slt_edit_end_qtr')
                                 , 'Qtr'
                                 , choices  = c('00','15','30','45')
                                 , selected = edit_fks()$end_qtr
                   )
          )
          , column(3
                   , uiOutput(ns('ot_ttl_hours'))
                   )

        )
      , fluidRow(
        column(12
               , div(class = 'whiteme'
               , uiOutput(ns('ot_edit_notes'))
               )
               )
      )
      , hr(style="border:2px solid #49688e")
      , fluidRow(
        column(6,
               actionButton(ns('btn_updt_db_edit_daily_hours'), 'update')
               )
      )



        # , tableOutput(ns('ot_edit_fks'))
      ))

    })




    # output$ot_edit_fks <- renderTable({
    #
    #   edit_fks()
    # })




    # observe({
    #
    #   print(input$rows)
    #
    # })

    # observeEvent(input$ot_res_fetch_hours_rows_selected,{
    #
    #   print('observed')
    #   print(input$ot_res_fetch_hours_rows_selected)
    #
    # })

    # observeEvent(input$edit_lastClick, {
    #
    #   print(input$edit_lastClickId)
    #
    # })



    #### <<<<    OUTPUTS         >>>>  ####
    #-------------------------------------#

    output$ot_edit_notes <- renderUI({

      tagList(
        textAreaInput(ns('txt_edit_notes')
                      , label = NULL
                      , value = edit_fks()$notes
        ) %>%
          shiny::tagAppendAttributes(style = 'width: 100%;')
      )

    })



    output$ot_ttl_hours <- renderUI({

      tagList(
        h5('Total Hours:')
        , h6(edit_values_all()$hours)
      )

    })

    # output$ot_res_fetch_hours <- renderTable({
    #
    #   res_fetch_hours()
    #
    # })

    output$ot_res_fetch_hours <- DT::renderDataTable({

      df_x <- res_fetch_hours() %>%
        dplyr::select(pk_id, date, account,  start, end_time, hours, notes)

      df_x[['actions']] <-
        paste0('
           <div class="btn-group" role="group" aria-label="Basic example">
           <button type="button" class="btn btn-secondary" style="background-color: lightsteelblue; margin: 2px" id=', df_x$pk_id,'>Edit Hours</button></div>
           ')

      DT::datatable(
        df_x
        , selection = "none"
        , rownames = FALSE
        , escape   = F
        , filter   = 'top'
        , options = list(
          server         = FALSE
          , dom          = 'frtip'
          , pageLength   = 40
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
