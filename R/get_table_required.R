#' Get required fields
#'
#' @param table name of table
#' @param key api key / token
#'
#' @return a dataframe of required fields and their properties
#' @export
#' @keywords internal
#'
get_table_required <- function(table, key){
  # if(! check_table_exists(table))
  #   stop("Unknown Table")

  get_fields(table, key) %>%
    filter(.data$required)
}
