

database_connection <- function(){

  DBI::dbConnect(
    RPostgres::Postgres()
    , dbname   = R.utils::getOption("db.dev.dbname")
    , host     = R.utils::getOption("db.dev.host")
    , port     = R.utils::getOption("db.dev.port")
    , user     = R.utils::getOption("db.dev.user")
    , password = R.utils::getOption("db.dev.password")
  )

}

# postgresdb <- database_connection()
# DBI::dbDisconnect(postgresdb)
