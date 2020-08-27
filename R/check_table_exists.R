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
  if(! table %in% get_tables())
    return(FALSE)
  return(TRUE)
}
