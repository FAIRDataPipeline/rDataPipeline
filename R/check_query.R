#' check_query
#'
#' Check whether entries exist in the database
#'
#' @param table
#' @param query
#'
#' @export
#'
check_query <- function(table, query) {

  response <- httr::GET(file.path("http://data.scrc.uk/api", table, ""),
                        query = query)

  tmp <- response %>%
    httr::content("text") %>%
    jsonlite::fromJSON(simplifyVector = FALSE)

  if(response$status_code == 200 & length(tmp) > 0) TRUE else FALSE
}
