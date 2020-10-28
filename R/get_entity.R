#' Get entity
#'
#' @param table a \code{string} specifying the name of the table
#' @param entity_id an \code{integer} specifying the id of an entry
#'
#' @export
#'
#' @examples
#' get_entity("storage_root", 11)
#'
get_entity <- function(table, entity_id) {
  if(!check_table_exists(table))
    stop(paste0("table doesn't exist"))
  if(missing(entity_id))
    stop("entity_id is required")
  if(is.na(suppressWarnings(as.integer(entity_id))))
    stop("entity_id must be an integer")

  httr::GET(paste("https://data.scrc.uk/api", table, entity_id, sep = "/")) %>%
    httr::content(as = "text", encoding = "UTF-8") %>%
    jsonlite::fromJSON(simplifyVector = FALSE)
}
