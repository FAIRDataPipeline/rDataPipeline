#' get_version_numbers
#'
#' @param a \code{string} specifying the name of the data product
#'
#' @return
#'
#' @export
#'
get_version_numbers <- function(name) {
  entries <- get_entry("data_product", list(name = name))
  version_numbers <- lapply(entries, function(x) x$version) %>% unlist()
}
