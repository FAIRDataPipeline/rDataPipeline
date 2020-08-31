#' Get Readable Fields
#'
#' @param table name of table
#' @param key api key / token
#' @return a dataframe of readable fields and their propertied
#'
#' @export
#'
get_table_readable <- function(table, key, type = FALSE){
  if(! check_table_exists(table))
    stop("Unknown Table")
  readable <- get_fields(table, key)
}
