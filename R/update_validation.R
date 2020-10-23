#' Private Functions to generate validation cache (inst/validation)
#'
#' @keywords internal
#'
update_validation <- function(key){
  try({
    if(missing(key))
      stop("key is required")
    # Save Tables
    tables <- get_tables(TRUE)
    if(!is.null(tables))
      saveRDS(tables, "inst/validation/tables.rds", version = 2)
    for(i in seq_along(tables)){
      table = tables[i]
      if(table == "users" | table == "groups"){
        #do nothing
      } else {
        saveRDS(get_fields(table, key, TRUE), paste0("inst/validation/", table, ".rds"), version = 2)
      }
    }
  })
}
