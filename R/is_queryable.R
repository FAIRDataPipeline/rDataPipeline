#' Check whether fields are queryable
#'
#' Check whether fields are queryable
#'
#' @param table a \code{string} specifying the name of the table
#' @param query a \code{list} containing the query
#'
#' @return Returns \code{TRUE} if the entry is queryable and \code{FALSE} if it
#' isn't
#' @export
#' @keywords internal
#'
is_queryable <- function(table, query) {

  # Check whether field names are valid

  fields <- names(query)
  valid_fields <- all(fields %in% get_table_queryable(table))

  # Check whether the class of each field in the query matches what is expected

  valid_query <- !(any(check_fields(table, query) == FALSE))

  # Output

  if (table == "users" | table == "groups") {
    # Only queryable with token
    usethis::ui_stop("Unable to query {ui_field(table)}")

  } else if (!check_table_exists(table)) {
    usethis::ui_stop("{ui_field(table)} does not exist")

  } else if (any(is.character(fields) & fields == "")) {
    # An empty string is not a valid field
    usethis::ui_stop("{ui_field(fields)} contains invalid fields")

  } else if (!valid_fields) {
    usethis::ui_stop("{ui_field(fields)} contains invalid fields")

  } else if (!valid_query) {
    usethis::ui_stop("query contains fields of incorrect class")

  } else {
    return(TRUE)
  }
}
