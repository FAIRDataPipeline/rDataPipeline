#' Get required fields
#'
#' @param table name of table
#'
#' @return a dataframe of required fields and their properties
#' @keywords internal
#'
get_table_required <- function(table) {
  # if(! check_table_exists(table))
  #   stop("Unknown Table")

  get_fields(table) %>%
    filter(.data$required)
}
