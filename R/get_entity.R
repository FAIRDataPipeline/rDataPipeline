#' get_entity
#'
#' @param table table
#' @param entity_id entity ID
#'
#' @export
#'
get_entity <- function(table, entity_id) {
  if(!check_table_exists(table))
    stop(paste0("table ", table, "does not exist"))
  if(missing(entity_id))
    stop("entity_id is required")
  if(is.na(suppressWarnings(as.integer(entity_id))))
    stop("entity_id must be an integer")
  out <- httr::GET(file.path("https://data.scrc.uk/api", table, entity_id)) %>%
    httr::content(as = "text", encoding = "UTF-8") %>%
    jsonlite::fromJSON(simplifyVector = FALSE)
}
