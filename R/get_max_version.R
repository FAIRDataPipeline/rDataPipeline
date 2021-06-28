#' get_max_version
#'
#' @param data_product
#' @param namespace_id
#'
get_max_version <- function(data_product, namespace_id) {

  entries <- get_entry("data_product",
                       list(name = data_product,
                            namespace = namespace_id))

  if (is.null(entries)) {
    usethis::ui_stop("Entry does not exist in local registry")
  }

  max_version <- lapply(entries, function(x) x$version) %>%
    unlist() %>%
    max() %>%
    semver::parse_version()
}
