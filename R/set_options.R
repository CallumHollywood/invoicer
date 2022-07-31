

# Set Session Options

# Topic: Database Connections
# --------------------------------

cnfg_db <- readr::read_csv('inst/app/cnfg/cnfg_db.csv')

lapply(1:nrow(cnfg_db), function(i){

  row_fks <- cnfg_db[i,]
  x       <- row_fks$x
  value   <- row_fks$value
  R.utils::setOption(x, value)

})

# --------------------------------
# --------------------------------


