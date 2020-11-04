#' Get data product version numbers
#'
#' Get all version numbers of a data product listed in the data registry
#'
#' @param data_product \code{string} specifying the name of the data product
#'
#' @export
#'
#' @examples
#' data_product <- "records/SARS-CoV-2/scotland/cases-and-management/testing"
#' get_version_numbers(data_product)
#'
get_version_numbers <- function(data_product) {
  entries <- get_entry("data_product", list(name = data_product))
  lapply(entries, function(x) x$version) %>% unlist()
}
