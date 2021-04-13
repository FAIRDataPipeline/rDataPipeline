#' Get entity from url
#'
#' @param url a \code{string} specifying the url of an entry
#'
#' @export
#' @keywords internal
#'
get_entity <- function(url) {
  # Sometimes an error is returned from the local registry:
  #   "Error in curl::curl_fetch_memory(url, handle = handle) :
  #    Failed to connect to localhost port 8000: Connection refused"
  # Repeating the action works eventually...
  continue <- TRUE
  i <- 1
  while (continue) {
    # Try retrieving entry
    tryCatch({
      output <- httr::GET(url) %>%
        httr::content(as = "text", encoding = "UTF-8") %>%
        jsonlite::fromJSON(simplifyVector = FALSE)
      continue <- FALSE
    },
    error = function(e) {
    },
    finally = {})
  }
  output
}
