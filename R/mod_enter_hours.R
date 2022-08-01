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
      column(3
             , actionButton(ns("btn_genr8_dayform")
                            , "GENR8 Day Form"
                            , width = '80%'
             )
      )
    )
    , br()
    , fluidRow(
      column(12
             , tags$div(id = ns("dyfrm_ui"))
             )
    )
  )
}

#' enter_hours Server Functions
#'
#' @noRd
mod_enter_hours_server <- function(
  id
  , pass_around
){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


    #### <<<<    GARGOYLE        >>>>  ####
    #-------------------------------------#





    #### <<<<    CALLMODULES     >>>>  ####
    #-------------------------------------#


    #### <<<<    STATIC VALUES   >>>>  ####
    #-------------------------------------#

    gen_dyfrms  <- reactiveValues()
    dyfrm_id    <- 1

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


    dstnct_accounts_rctv <- eventReactive(id,{

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


    observeEvent(input$btn_genr8_dayform, {

      x_id <- paste0("dyfrm_", dyfrm_id)


      gen_dyfrms[[x_id]] <- mod_genr8_day_form_server(
        id = x_id
        , genr8_dayform      = reactive({input$btn_genr8_dayform})
        , dstnct_accounts_in = reactive({dstnct_accounts_rctv()})
        , pass_around        = pass_around
      )

      insertUI(
        selector = paste0("#", ns(paste0("dyfrm_ui")))
        , ui = mod_genr8_day_form_ui(
          ns(x_id)
          )
        )

      dyfrm_id <<- dyfrm_id + 1


    })


    #### <<<<    OUTPUTS         >>>>  ####
    #-------------------------------------#

    #### <<<<    OUTPUT OPTIONS  >>>>  ####
    #-------------------------------------#




  })
}
