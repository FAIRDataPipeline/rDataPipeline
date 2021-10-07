#' Get readable fields
#'
#' @param table name of table
#' @param endpoint a \code{string} specifying the registry endpoint
#'
#' @return a dataframe of readable fields and their properties
#' @keywords internal
#'
get_table_readable <- function(table, endpoint) {
  # if(! check_table_exists(table))
  #   stop("Unknown Table")
  get_fields(table, endpoint)
}
