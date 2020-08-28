#' Get tables from registry
#'
#' Use api endpoint to produce a list of tables
#'
#' @return a character vector of tables
#'
#' @export
#'
get_tables <- function(){
  out <- httr::GET(paste("https://data.scrc.uk/api", "", sep = "/")) %>%
    httr::content(as = "text", encoding = "UTF-8") %>%
    jsonlite::fromJSON(simplifyVector = FALSE)
  return(names(out))
}
