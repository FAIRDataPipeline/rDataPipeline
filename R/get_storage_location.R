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
#' get_storage_location(10404)
#' }
#'
get_storage_location <- function(location){
  get_entity("storage_location", basename(location))
}
