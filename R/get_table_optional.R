#' Get optional fields
#'
#' @param table a \code{string} specifying the name of the table
#'
#' @return Returns a \code{data.frame} of optional fields and thier properties
#' @export
#' @keywords internal
#'
get_table_optional <- function(table){
  # if(! check_table_exists(table))
  #   stop("Unknown Table")
  optional <- get_fields(table) %>%
    filter(!.data$read_only) %>%
    filter(!.data$required)
}
