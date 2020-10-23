#' Get tables from registry
#'
#' Use api endpoint to produce a list of tables
#' @param live whether or not to get the tables directly from the API
#' @return a character vector of tables
#'
#' @export
#'
#' @keywords internal
#'
get_tables <- function(live = FALSE){
  tables.file = system.file("validation", "tables.rds", package = "SCRCdataAPI")
  if(tables.file == "" | live)
  {
    out <- httr::GET(paste("https://data.scrc.uk/api", "", sep = "/")) %>%
    httr::content(as = "text", encoding = "UTF-8") %>%
    jsonlite::fromJSON(simplifyVector = FALSE)
    return(names(out))
  }
    return(readRDS(tables.file))

}
