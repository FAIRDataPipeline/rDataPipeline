#' get_max_version
#'
#' If entry doesn't exist in the registry, return version 0.0.0
#'
#' @param data_product data_product
#' @param namespace_id namespace_id
#'
get_max_version <- function(data_product, namespace_id) {

  entries <- get_entry("data_product",
                       list(name = data_product,
                            namespace = namespace_id))

  if (is.null(entries)) {
    max_version <- semver::parse_version("0.0.0")
  } else {
    max_version <- lapply(entries, function(x) x$version) %>%
      unlist() %>%
      max() %>%
      semver::parse_version()
  }
  max_version
}
