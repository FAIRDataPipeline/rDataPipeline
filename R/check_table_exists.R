#' check table exists
#'
#' @param table name of table
#'
#' @return boolean if a table exists
#'
#' @export
#'
check_table_exists <- function(table){
  if(missing(table))
    stop("Table is a required paramater")
  if(!is.character(table))
    stop("Table must be a string")
  tables <- NULL
  tables.file <- system.file("validation", "tables.rds", package = "SCRCdataAPI")
  if(tables.file == ""){
    tables <- get_tables()
  } else {
    tables <- readRDS(tables.file)
  }

  if(! table %in% tables)
    return(FALSE)
  return(TRUE)
}
