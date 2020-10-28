#' check_query
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
check_query <-function(table, query){
#'
#' @examples
#' check_query("storage_root", list(name = "github"))
#'
  if(!is.list(query))
    stop("Invalid query type")
  if(length(query) == 0)
    return(TRUE)
  return(all(is_queryable(table, names(query))))
}
