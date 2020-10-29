#' Check if query is valid
#'
#' Check if query is valid
#'
#' @param table a \code{string} specifying the name of the table
#' @param query a \code{list} containing a valid query for the table, *e.g.*
#' \code{list(field = value)}
#'
#' @return Returns \code{TRUE} if the query is valid for the table, \code{FALSE}
#' if it isn't
#' @export
#' @keywords internal
#'
#' @examples
#' check_query("storage_root", list(name = "github"))
#' check_query("storage_root", list(not_a_field = "github"))
#' check_query("not_a_table", list(not_a_field = "github"))
#'
check_query <- function(table, query){
  if(!is.list(query)) {
    message("query should be a list")
    return(FALSE)
  }

  # An empty list is valid
  if(length(query) == 0)
    return(TRUE)

  all(is_queryable(table, names(query)))
}
