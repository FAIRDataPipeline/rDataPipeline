#' check_dataproduct_exists
#'
#' If a data product already exists with the same name, version, and
#' namespace, throw an error
#'
#' @param write_dataproduct write_dataproduct
#' @param write_version write_version
#' @param write_namespace_id write_namespace_id
#' @param endpoint endpoint
#'
check_dataproduct_exists <- function(write_dataproduct,
                                     write_version,
                                     write_namespace_id,
                                     endpoint) {

  check_exists <- get_entry("data_product",
                            list(name = write_dataproduct,
                                 version = write_version,
                                 namespace = write_namespace_id),
                            endpoint = endpoint)

  if (!is.null(check_exists))
    usethis::ui_stop("A data product with the same name ({write_dataproduct}), version ({write_version}), and namespace ({write_namespace}) already exists")
}
