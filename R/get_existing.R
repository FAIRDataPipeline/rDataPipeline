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
                         key) {

  tmp <- httr::GET(file.path(endpoint, table, ""),
                   httr::add_headers(.headers = headers)) %>%
    httr::content("text") %>%
    jsonlite::fromJSON(simplifyVector = FALSE)

  output <- lapply(tmp, function(x) x$name) %>%
    unlist(use.names = FALSE)

  if(!missing(key))
    output <- tmp[[which(output == key)]]

  output
}
