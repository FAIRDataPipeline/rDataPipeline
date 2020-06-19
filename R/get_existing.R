#' get_existing
#'
#' @param endpoint api endpoint
#' @param table
#' @param user_keys
#'
#' @export
#'
get_existing <- function(endpoint = "http://data.scrc.uk/api",
                         table,
                         user_keys = "name") {

  out <- httr::GET(file.path(endpoint, table)) %>%
    httr::content("text") %>%
    jsonlite::fromJSON(simplifyVector = FALSE)

  if(!missing(user_keys))
    lapply(out, function(x) x[user_keys]) %>% unlist(use.names = FALSE)

  out
}
