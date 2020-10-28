#' Get Storage Location
#'
#' Get storage location entry
#'
#' @param location_id an \code{integer} specifying the id of an entry in the
#' storage_location table
#'
#' @return Returns a \code{list} of fields associated with the specified entry
#' @keywords internal
#'
#' @examples
#' get_storage_location(10404)
#'
get_storage_location <- function(location_id){
  get_entity("storage_location", location_id)
}
