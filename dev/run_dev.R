# options(golem.app.prod = FALSE) # TRUE = production mode, FALSE = development mode
 options(shiny.port = httpuv::randomPort())
golem::detach_all_attached()
# rm(list=ls(all.names = TRUE))
golem::document_and_reload()
run_app()
