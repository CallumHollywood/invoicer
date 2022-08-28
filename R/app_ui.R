#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd

# options(browser = "C:/Program Files/Mozilla Firefox/firefox.exe")

# source('R/set_options.R')

app_ui <- function(request) {
  tagList(
    golem_add_external_resources(),
    bs4Dash::bs4DashPage(
      dark = NULL,
      freshTheme = fresh::create_theme(
        fresh::bs4dash_vars(
          navbar_light_color = "#bec5cb",
          navbar_light_active_color = "#FFF",
          navbar_light_hover_color = "#FFF"
        ),
        fresh::bs4dash_yiq(
          contrasted_threshold = 10,
          text_dark = "#FFF",
          text_light = "#272c30"
        ),
        fresh::bs4dash_layout(
          main_bg = "#353c42"
        ),
        fresh::bs4dash_sidebar_light(
          bg = "#272c30",
          color = "#bec5cb",
          hover_color = "#FFF",
          submenu_bg = "#272c30",
          submenu_color = "#FFF",
          submenu_hover_color = "#FFF"
        ),
        fresh::bs4dash_status(
          primary = "#5E81AC", danger = "#BF616A", light = "#272c30"
        ),
        fresh::bs4dash_color(
          gray_900 = "#FFF", white = "#272c30"
        )
      ),
      options = NULL,
      header = bs4Dash::dashboardHeader(
        title = bs4Dash::dashboardBrand(
          # title = "Refraction",
          title = 'TBD Title', # htmlOutput('outpt_title'),
          color = "primary",
          image = 'www/tbd.png'
        )
        , shinyauthr::logoutUI("logout", style = 'color: black')
      ),

      sidebar = bs4Dash::dashboardSidebar(

        bs4Dash::sidebarMenuOutput("outpt_sidebar")

      ),
      body = bs4Dash::dashboardBody(
        div( class = "login-ui"
             , shinyauthr::loginUI(
               "login"
               , user_title    = "callum"
               , pass_title    = "pw_callu_78"
               , login_title   = div('LOGIN', style = 'color: black')
               , cookie_expiry = cookie_expiry
             )),
        uiOutput("outpt_main")
      ),
      controlbar = bs4Dash::dashboardControlbar(
        mod_cntrl_bar_ui("cntrl_bar_ui_1")
      ),
      title = "authapp",

    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "authapp"
    )
    , shinyjs::useShinyjs()
    , tags$style(".login-ui input { color: #fff}")
    , tags$style(".login-ui input:focus { color: #fff}")
    , tags$style(".login-ui textarea.form-control { color: #fff}")
    # , shinyalert::useShinyalert()
  )
}


# mckleijn_stg
# ---------------------------
# con <- pool::dbPool(
#   drv = RPostgres::Postgres(),
#   dbname = "mckleijn_dev",
#   host = "localhost",
#   user = "postgres",
#   password = "postgres_password"
# )

# mckleijn_stg refraction
# ---------------------------
# con <- pool::dbPool(
#   drv = RPostgres::Postgres(),
#   dbname   = "mckleijn_stg",
#   host     = "localhost",
#   user     = "admin",
#   password = "kr1k3ybanana"
# )

# con <- pool::dbPool(
#   drv = RPostgres::Postgres(),
#   dbname   = "mckleijn_stg",
#   host     = "102.67.140.113",
#   user     = "postgres",
#   password = "noisybird1"
# )



# onStop(function() {
#   pool::poolClose(con)
# })

# con <- appbench::database_connection()
# user_base <- DBI::dbGetQuery(con, 'SELECT * FROM accounts.user_base')
# DBI::dbDisconnect(con)

