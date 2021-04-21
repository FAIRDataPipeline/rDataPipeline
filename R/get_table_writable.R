#' Get writable fields
#'
#' @param table a \code{string} specifying the name of the table
#' @param key api key / token
#'
#' @return Returns a character vector of writable fields
#' @export
#' @keywords internal
#'
get_table_writable <- function(table, key){
  # if(!check_table_exists(table))
  #   stop("Unknown Table")

  get_fields(table, key) %>%
    filter(!.data$read_only)
}
