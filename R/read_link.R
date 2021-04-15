#' read_link
#'
#' @param handle list
#' @param alias string
#'
#' @export
#'
read_link <- function(handle, alias) {
  # Get entries
  url <- unname(handle$inputs[[alias]])
  run_server()
  external_object <- get_entity(url)
  object <- get_entity(external_object$object)
  storage_location <- get_entity(object$storage_location)
  storage_root <- get_entity(storage_location$storage_root)
  stop_server()

  # Return storage location
  invisible(file.path(storage_root$root, storage_location$path))
}
