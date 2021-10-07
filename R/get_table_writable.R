#' Get writable fields
#'
#' @param table a \code{string} specifying the name of the table
#' @param endpoint a \code{string} specifying the registry endpoint
#'
#' @return Returns a character vector of writable fields
#' @keywords internal
#'
get_table_writable <- function(table, endpoint) {
  # if(!check_table_exists(table))
  #   stop("Unknown Table")

  get_fields(table, endpoint) %>%
    filter(!.data$read_only)
}
