#' Check if Query is Valid
#'
#' Produces an error if table does not exist or if query is not a list
#'
#' @param table a \code{string} specifying the name of the table
#' @param query a \code{list} containing a valid query for the table, *e.g.*
#' \code{list(field = value)}
#'
#' @return Returns boolean if the query is valid for the table
#'
#' @export
#'
#' @keywords internal
#'
#' @examples
#' check_query("storage_root", list(name = "github"))
#'
check_query <- function(table, query){
  if(!is.list(query))
    stop("query should be a list")

  # An empty list is valid
  if(length(query) == 0)
    return(TRUE)

  all(is_queryable(table, names(query)))
}
