#' Get URL
#'
#' Retrieve URLs for particular entries or all entries in a table
#'
#' @param table a \code{string} specifying the name of the table
#' @param query a \code{list} containing a valid query for the table, *e.g.*
#' \code{list(field = value)}
#' @param endpoint a \code{string} specifying the registry endpoint
#'
#' @return Returns a string or list of strings specifying the URL or URLs of
#' entries in a table
#'
get_url <- function(table,
                    query = list(),
                    endpoint = "http://localhost:8000/api/") {

  output <- get_entry(table = table,
                      query = query,
                      endpoint = endpoint)

  if (length(output) == 1) {
    return(output[[1]]$url)
  } else if (length(output) == 0) {
    return(NULL)
  } else {
    return(lapply(output, function(x) x$url))
  }
}
