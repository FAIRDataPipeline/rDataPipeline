#' Get Writable Fields
#'
#' @param table name of table
#' @param key api key / token
#' @return a character vector of writable fields
#'
#' @export
#'
get_table_writable <- function(table, key){
  if(! check_table_exists(table))
    stop("Unknown Table")

  get_fields(table, key) %>%
    filter(!.data$read_only)
}
