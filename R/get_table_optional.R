#' Get optional fields
#'
#' @param table a \code{string} specifying the name of the table
#' @param endpoint a \code{string} specifying the registry endpoint
#'
#' @return Returns a \code{data.frame} of optional fields and their properties
#' @keywords internal
#'
get_table_optional <- function(table, endpoint) {
  # if(! check_table_exists(table))
  #   stop("Unknown Table")
  optional <- get_fields(table, endpoint) %>%
    filter(!.data$read_only) %>%
    filter(!.data$required)
}
