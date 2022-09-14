#' genr8_hrs_form UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList


fn_sn_env_sn_roles <- function(tbl){

  tbl_role_id <- as.list(tbl$role_id)

  names(tbl_role_id) <- tbl$role

  return(tbl_role_id)

}

mod_genr8_hrs_form_ui <- function(
    id
    , dstnct_accounts_in
    , sn_env
){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(4
             , style = 'background-color: #707987;'
             , fluidRow(
               column(3
                      , checkboxInput(ns('chbx_incl_hrs')
                                      , 'Include'
                                      , value = T
                      )
               )
               , column(9
                        , fluidRow(
                        selectInput(ns("slt_account")
                                      , 'account'
                                      , choices = dstnct_accounts_in
                        )

                        )
                        , fluidRow(
                          selectInput(ns("slt_role")
                                      , 'role'
                                      # , choices = sn_env$sn_roles$role_id
                                      # , choices = list('developer' = 1, 'consultant' = 2)
                                      # , choices = list(as.list(setNames(sn_env$sn_roles$role, sn_env$sn_roles$role))))
                                      , choices = fn_sn_env_sn_roles(sn_env$sn_roles)
                          )

                        )
               )
             )
      )
      , column(4
               , style = 'background-color: #c3d0e6;'
               , fluidRow(
                 column(6
                        , selectInput(ns('slt_strt_hr')
                                      , 'Hour'
                                      , choices  = c(paste0('0', 0:9), 10:23)
                                      , selected = '08'
                        )

                 )
                 , column(6
                          , selectInput(ns('slt_strt_qtr')
                                        , 'Qtr'
                                        , choices  = c('00','15','30','45')
                                        , selected = '00'
                          )
                 )
               )
      )
      , column(4
               , style = 'background-color: #707987;'
               , fluidRow(
                 column(4
                        , selectInput(ns('slt_end_hr')
                                      , 'Hour'
                                      , choices  = c(paste0('0', 0:9), 10:23)
                                      , selected = '13'
                        )

                 )
                 , column(4
                          , selectInput(ns('slt_end_qtr')
                                        , 'Qtr'
                                        , choices  = c('00','15','30','45')
                                        , selected = '00'
                          )
                 )
                 , column(4
                          , uiOutput(ns('ot_dur'))
                 )
               )
      )

    )
    , fluidRow(
      column(12
             , div(class = 'login-ui'
                   , textAreaInput(ns('txt_notes'), label = NULL, width = '100%')
             )
      )
    )
  )
}

#' genr8_hrs_form Server Functions
#'
#' @noRd
mod_genr8_hrs_form_server <- function(
    id
    , dt_entr_day
){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


    #### <<<<    CALLMODULES     >>>>  ####
    #-------------------------------------#

    #### <<<<    STATIC VALUES   >>>>  ####
    #-------------------------------------#


    #### <<<<   REACTIVES        >>>>  ####
    #-------------------------------------#

    select_values_all <- reactive({

      req(dt_entr_day())
      req(input$slt_strt_hr)
      req(input$slt_strt_qtr)
      req(input$slt_end_hr)
      req(input$slt_end_qtr)
      req(input$slt_account)
      req(input$slt_role)

      tibble::tibble(
        id = as.numeric(stringr::str_remove(id, 'dyfrm_' ))
        ,
        dt_entr_day = isolate(dt_entr_day())
        ,
        account  = input$slt_account
        , strt_hr  = input$slt_strt_hr
        , strt_qtr = input$slt_strt_qtr
        , end_hr   = input$slt_end_hr
        , end_qtr  = input$slt_end_qtr
        , incl_hrs = input$chbx_incl_hrs
        , notes    = input$txt_notes
        , role_id  = input$slt_role
      ) %>%

        dplyr::rename(date = dt_entr_day) %>%
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
        dplyr::mutate(hours = as.numeric(difftime(date_end,date_start, units = "hours"))) %>%
        dplyr::filter(incl_hrs == T)


    })


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

    output$ot_dur <- renderUI({

      tagList(
        h5('Hrs')
        , h5(select_values_all()$hours)
      )

    })

    output$ot_slt_account <- renderUI({

      tagList(
        selectInput(ns("slt_account")
                    , 'account'
                    , choices = isolate(dstnct_accounts_in())
        )

      )

    })

    #### <<<<    OUTPUT OPTIONS  >>>>  ####
    #-------------------------------------#


    #### <<<<    RETURNS         >>>>  ####
    #-------------------------------------#

    return(list(
      select_values_all = reactive({select_values_all()})
    ))

  })
}
