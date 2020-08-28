#' Get Readable Fields
#'
#' @param table name of table
#' @param key api key / token
#' @return a character vector of readable fields
#'
#' @export
#'
get_table_readable <- function(table, key){
  if(! check_table_exists(table))
    stop("Unknown Table")
  get_fields(table, key, "all")
}