#' extract_id
#' 
#' @keywords internal
#'
#' @param url url
#' @param endpoint endpoint
#'
extract_id <- function(url, endpoint) {
  gsub(paste0(endpoint, ".*/([0-9]*)/"), "\\1", url)
}
