#' is_queryable
#'
#' Produces error \code{Unknown Table} if table does not exist
#'
#' @param table name of table
#' @param query_parameter a string or vector of strings to check
#'
#' @return either true / false if a single string is provided or a vector or
#' true or false if vector is provided
#'
#' @export
#'
#' @keywords internal
#'
is_queryable <- function(table, query_parameter) {
  if(table == "users" | table == "groups")
    return(FALSE) # only queryable with token
  if(is.null(get_table_queryable(table)))
    return(FALSE)
  return(query_parameter %in% get_table_queryable(table))
}
