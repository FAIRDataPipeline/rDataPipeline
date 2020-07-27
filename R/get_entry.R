#' get_entry
#'
#' @param table table
#' @param query query
#'
#' @export
#'
get_entry <- function(table, query = list()) {
  out <- httr::GET(file.path("https://data.scrc.uk/api", table, ""),
            query = query) %>%
    httr::content(as = "text", encoding = "UTF-8") %>%
    jsonlite::fromJSON(simplifyVector = FALSE)
  out$results[[1]]
}
