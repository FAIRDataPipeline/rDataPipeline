#' Get Required Fields
#'
#' @param table name of table
#' @param key api key / token
#'
#' @return a character vector of required fields
#'
#' @export
#'
get_table_required <- function(table, key){
  if(! check_table_exists(table))
    stop("Unknown Table")
  return(get_fields(table, key, "required"))
}
