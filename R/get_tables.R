#' Get tables from registry
#'
#' Use api endpoint to produce a list of tables
#' @param endpoint a \code{string} specifying the registry endpoint
#'
#' @return a character vector of tables
#' @keywords internal
#'
get_tables <- function(endpoint = "http://127.0.0.1:8000/api/") {

  httr::GET(endpoint) %>%
    httr::content(as = "text", encoding = "UTF-8") %>%
    jsonlite::fromJSON(simplifyVector = FALSE) %>%
    names()
}
