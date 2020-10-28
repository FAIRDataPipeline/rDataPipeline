#' get_url
#'
#' @param table a \code{string} specifying the name of the table
#' @param query a \code{list} containing a valid query for the table, *e.g.*
#' \code{list(field = value)}
#'
#' @export
#'
#' @examples
#' get_url("storage_root")
#'
get_url <- function(table, query = list()) {

  tmp <- httr::GET(paste("https://data.scrc.uk/api", table, "", sep = "/"),
                   query = query) %>%
    httr::content(as = "text", encoding = "UTF-8") %>%
    jsonlite::fromJSON(simplifyVector = FALSE)

  tmp <- tmp$results

  if(length(tmp) == 1) {
    return(tmp[[1]]$url)
  } else if(length(tmp) == 0) {
    stop("No objects were returned")
  } else {
    return(lapply(tmp, function(x) x$url))
  }

}
