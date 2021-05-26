#' extract_id
#'
#' @param url url
#'
extract_id <- function(url) {
gsub("http://localhost:8000/api/.*/([0-9]*)/", "\\1", url)
}
