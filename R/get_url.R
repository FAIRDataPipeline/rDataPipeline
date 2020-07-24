#' get_url
#'
#' @param table table
#' @param query query
#'
#' @export
#'
get_url <- function(table, query = list()) {

  tmp <- httr::GET(file.path("https://data.scrc.uk/api", table, ""),
                   query = query) %>%
    httr::content(as = "text", encoding = "UTF-8") %>%
    jsonlite::fromJSON(simplifyVector = FALSE)

  tmp <- tmp$results[[1]]

  if(length(tmp) == 1) {
    return(tmp[[1]]$url)
  } else if(length(tmp) == 0) {
    stop("No objects were returned")
  } else {
    return(lapply(tmp, function(x) x$url))
  }

}