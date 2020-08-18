#' check_query
#' produces error if table does not exist or if query is not a list
#'
#' @param table name of table
#' @param query query to check
#'
#' @return boolean if the query is valid for the table
#'
#' @export
#'
check_query <-function(table, query){
  if(!is.list(query))
    stop("Invalid query type")
  if(length(query) == 0)
    return(TRUE)
  return(all(is_queryable(table, names(query))))
}
