#' Get storage location from url
#'
#' Get storage location entry
#'
#' @param location the url of an entry in the storage_location table
#'
#' @return Returns a \code{list} of fields associated with the specified entry
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' get_storage_location("http://localhost:8000/api/storage_location/258/")
#' }
#'
get_storage_location <- function(location){
  tmp <- get_entity(location)
  paste0(tmp$storage_root, tmp$path)
}
