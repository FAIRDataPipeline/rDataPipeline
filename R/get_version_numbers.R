#' get_version_numbers
#'
#' @param data_product \code{string} specifying the name of the data product
#'
#' @return
#' @export
#'
get_version_numbers <- function(data_product) {
  entries <- get_entry("data_product", list(name = data_product))
  lapply(entries, function(x) x$version) %>% unlist()
}
