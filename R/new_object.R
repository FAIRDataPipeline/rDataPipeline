#' new_object
#'
#' @param storage_location_id
#' @param key
#'
#' @export
#'
new_object <- function(storage_location_id,
                       key) {

  # store_url <- get_url("storage_location", list(path = storage_location))

  post_data(table = "object",
            data =  list(storage_location = storage_location_id),
            key)
}
