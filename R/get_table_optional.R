#' Get Optional Fields
#'
#' @param table name of table
#' @param key api key / token
#' @return a dataframe of optional fieldsa and thier properties
#'
#' @export
#'
get_table_optional <- function(table, key){
  if(! check_table_exists(table))
    stop("Unknown Table")
  optional <- get_fields(table, key) %>%
    filter(!.data$read_only) %>%
    filter(!.data$required)
}
