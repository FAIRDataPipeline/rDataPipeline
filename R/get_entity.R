#' Get entity from url
#' 
#' @keywords internal
#'
#' @param url a \code{string} specifying the url of an entry
#'
get_entity <- function(url) {

  key <- get_token()
  h <- c(Authorization = paste("token", key))

  # Sometimes an error is returned from the local registry:
  #   "Error in curl::curl_fetch_memory(url, handle = handle) :
  #    Failed to connect to 127.0.0.1:8000 port 8000: Connection refused"
  # Repeating the action works eventually...
  continue <- TRUE
  while (continue) {
    tryCatch({ # Try retrieving entry
      output <- httr::GET(url,
                          httr::add_headers(.headers = h)) %>%
        httr::content(as = "text", encoding = "UTF-8") %>%
        jsonlite::fromJSON(simplifyVector = FALSE)
      continue <- FALSE
    },
    error = function(e) {
    })
  }
  output
}
