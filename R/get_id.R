#' Get ID
#'
#' Retrieve IDs for particular entries or all entries in a table
#'
#' @param table a \code{string} specifying the name of the table
#' @param query a \code{list} containing a valid query for the table, *e.g.*
#' \code{list(field = value)}
#' @param endpoint a \code{string} specifying the registry endpoint
#'
#' @return Returns a string or list of strings specifying the URL or URLs of
#' entries in a table
#'
get_id <- function(table,
                   query = list(),
                   endpoint = "http://localhost:8000/api/") {

  output <- get_entry(table, query, endpoint)

  if (length(output) == 1) {
    return(extract_id(output[[1]]$url, endpoint = endpoint))
  } else if (length(output) == 0) {
    return(NULL)
  } else {
    return(lapply(output, function(x) extract_id(x$url, endpoint = endpoint)))
  }
}
