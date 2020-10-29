#' Check if table exists
#'
#' Check if table exists in the data registry
#'
#' @param table a \code{string} specifying the name of the table
#'
#' @return Returns \code{TRUE} if a table exists, \code{FALSE} if it doesn't
#' @export
#' @keywords internal
#'
check_table_exists <- function(table){
  if(missing(table))
    stop("table is a required paramater")
  if(!is.character(table))
    stop("table must be a string")
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
