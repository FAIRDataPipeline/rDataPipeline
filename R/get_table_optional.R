#' Get optional fields
#'
#' @param table a \code{string} specifying the name of the table
#' @param key API token from data.scrc.uk
#'
#' @return Returns a \code{data.frame} of optional fields and thier properties
#' @export
#' @keywords internal
#'
get_table_optional <- function(table, key){
  # if(! check_table_exists(table))
  #   stop("Unknown Table")
  optional <- get_fields(table, key) %>%
    filter(!.data$read_only) %>%
    filter(!.data$required)
}
