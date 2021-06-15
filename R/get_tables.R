#' Get tables from registry
#'
#' Use api endpoint to produce a list of tables
#' @param live whether or not to get the tables directly from the API
#'
#' @return a character vector of tables
#' @export
#' @keywords internal
#'
get_tables <- function(live = FALSE){
  httr::GET(paste("http://localhost:8000/api", "", sep = "/")) %>%
    httr::content(as = "text", encoding = "UTF-8") %>%
    jsonlite::fromJSON(simplifyVector = FALSE) %>%
    names()
}
