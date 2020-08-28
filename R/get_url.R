#' get_url
#'
#' @param table table
#' @param query query
#'
#' @export
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
