#' get_storage_location
#'
#' @param location_id Location ID
#'
#' @return a list of fields from a location_id or null if there was an error
#'
#' @export
#'
get_storage_location <- function(location_id){
  get_entity("storage_location", location_id)
}
