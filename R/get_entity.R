#' get_entity
#'
#' @param table table
#' @param entity_id entity ID
#'
#' @export
#'
get_entity <- function(table, entity_id) {
  out <- httr::GET(file.path("https://data.scrc.uk/api", table, entity_id)) %>%
    httr::content(as = "text", encoding = "UTF-8") %>%
    jsonlite::fromJSON(simplifyVector = FALSE)
}
