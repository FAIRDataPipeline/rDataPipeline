#' extract_id
#'
extract_id <- function(url) {
gsub("http://localhost:8000/api/.*/([0-9]*)/", "\\1", url)
}
