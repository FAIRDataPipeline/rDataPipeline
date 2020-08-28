#' Get Optional Fields
#'
#' @param table name of table
#' @param key api key / token
#' @param type whether to return type of field, returns data frame
#' @return a character vector of optional fields
#'
#' @export
#'
get_table_optional <- function(table, key, type = FALSE){
  if(! check_table_exists(table))
    stop("Unknown Table")
  get_fields(table, key, "optional", type)
}
