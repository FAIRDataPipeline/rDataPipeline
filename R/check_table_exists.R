#' check table exists
#'
#' @param table name of table
#'
#' @return boolean if a table exists
#'
#' @export
#'
check_table_exists <- function(table){
  if(! table %in% get_tables())
    return(FALSE)
  return(TRUE)
}
