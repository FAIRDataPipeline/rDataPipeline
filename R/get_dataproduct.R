#' get_dataproduct
#'
#' @param data_product data_product
#' @param version version
#' @param namespace namespace
#' @param endpoint endpoint
#'
#' @export
#'
get_dataproduct <- function(data_product,
                            version,
                            namespace,
                            endpoint = "http://localhost:8000/api/") {
  # Get provenance URL
  namespace_entry <- get_entry("namespace",
                               list(name = namespace),
                               endpoint = endpoint)
  assertthat::assert_that(length(namespace_entry) == 1)
  namespace_url <- namespace_entry[[1]]$url
  namespace_id <- gsub(paste0(endpoint, ".*/([0-9]*)/"), "\\1", namespace_url)

  dp_entry <- get_entry("data_product",
                        list(name = data_product,
                             version = version,
                             namespace = namespace_id),
                        endpoint = endpoint)

  object <- get_entity(dp_entry[[1]]$object)
  storage_location <- get_entity(object$storage_location)
  storage_root <- get_entity(storage_location$storage_root)
  storage_root <- storage_root$root
  storage_root <- gsub("file://", "", storage_root)

  file.path(storage_root, storage_location$path)
}
