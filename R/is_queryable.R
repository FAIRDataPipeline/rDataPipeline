#' Check whether fields are queryable
#'
#' Check whether fields are queryable
#'
#' @param table a \code{string} specifying the name of the table
#' @param query a \code{string} or \code{vector} of field names
#'
#' @return Returns \code{TRUE} if the entry is queryable and \code{FALSE} if it
#' isn't
#' @export
#' @keywords internal
#'
is_queryable <- function(table, query) {

  valid_fields <-  all(query %in% get_table_queryable(table))

  if (table == "users" | table == "groups") {
    # Only queryable with token
    return(FALSE)

  } else if (!check_table_exists(table)) {
    message("Table doesn't exist")
    return(FALSE)

  } else if (any(is.character(query) & query == "")) {
    # An empty string is not a valid query
    return(FALSE)

  } else if (!valid_fields) {
    return(FALSE)

  } else {
    return(TRUE)
  }
}
