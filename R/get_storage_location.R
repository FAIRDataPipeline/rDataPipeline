#' Get storage location from url
#'
#' Get storage location entry
#'
#' @param location the url of an entry in the storage_location table
#'
#' @return Returns a \code{list} of fields associated with the specified entry
#'
get_storage_location <- function(location) {
  tmp <- get_entity(location)
  path <- tmp$path
  root <- get_entity(tmp$storage_root)$root
  paste0(root, path)
}
