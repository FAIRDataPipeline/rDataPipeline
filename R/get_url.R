#' Get URL
#'
#' Retrieve URLs for particular entries or all entries in a table
#'
#' @param table a \code{string} specifying the name of the table
#' @param query a \code{list} containing a valid query for the table, *e.g.*
#' \code{list(field = value)}
#'
#' @return Returns a string or list of strings specifying the URL or URLs of
#' entries in a table
#' @export
#'
#' @examples
#' # Retrieve URLs for all entries in a table
#' get_url(table = "storage_root")
#'
#' # Retrieve the URL of a particular entry in a table
#' get_url(table = "storage_root", list(name = "github"))
#'
get_url <- function(table, query = list()) {

  tmp <- httr::GET(paste("http://localhost:8000/api", table, "", sep = "/"),
                   query = query) %>%
    httr::content(as = "text", encoding = "UTF-8") %>%
    jsonlite::fromJSON(simplifyVector = FALSE)

  tmp <- tmp$results

  if(length(tmp) == 1) {
    return(tmp[[1]]$url)
  } else if(length(tmp) == 0) {
    stop("No objects were returned")
  } else {
    return(lapply(tmp, function(x) x$url))
  }
}
