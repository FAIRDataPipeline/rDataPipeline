#' Get readable fields
#'
#' @param table name of table
#'
#' @return a dataframe of readable fields and their properties
#' @export
#' @keywords internal
#'
get_table_readable <- function(table){
  # if(! check_table_exists(table))
  #   stop("Unknown Table")
  readable <- get_fields(table)
}
