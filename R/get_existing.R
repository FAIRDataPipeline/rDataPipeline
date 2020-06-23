#' get_existing
#'
#' @param table
#' @param key
#' @param endpoint api endpoint
#'
#' @export
#'
#' @examples
#' get_existing(table = "storage_type")
#' get_existing(table = "storage_type", key = "ftp")
#'
get_existing <- function(table,
                         key,
                         endpoint = "http://data.scrc.uk/api") {

  tmp <- httr::GET(file.path(endpoint, table, "")) %>%
    httr::content("text") %>%
    jsonlite::fromJSON(simplifyVector = FALSE)

  output <- lapply(tmp, function(x) x$name) %>%
    unlist(use.names = FALSE)

  if(!missing(key))
    output <- tmp[[which(output == key)]]

  output
}
