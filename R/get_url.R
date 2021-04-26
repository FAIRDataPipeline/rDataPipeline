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
#' \dontrun{
#' # Retrieve URLs for all entries in a table
#' get_url(table = "storage_root")
#'
#' # Retrieve the URL of a particular entry in a table
#' get_url(table = "storage_root", list(name = "github"))
#' }
#'
get_url <- function(table, query = list()) {

  # Sometimes an error is returned from the local registry:
  #   "Error in curl::curl_fetch_memory(url, handle = handle) :
  #    Failed to connect to localhost port 8000: Connection refused"
  # Repeating the action works eventually...
  continue <- TRUE
  while (continue) {
    tryCatch({ # Try retrieving entry
      tmp <- httr::GET(paste0("http://localhost:8000/api/", table, ""),
                       query = query) %>%
        httr::content(as = "text", encoding = "UTF-8") %>%
        jsonlite::fromJSON(simplifyVector = FALSE)
      continue <- FALSE
    },
    error = function(e) {
    })
  }

  tmp <- tmp$results

  if(length(tmp) == 1) {
    return(tmp[[1]]$url)
  } else if(length(tmp) == 0) {
    stop("No objects were returned")
  } else {
    return(lapply(tmp, function(x) x$url))
  }
}
