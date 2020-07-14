#' new_object
#'
#' @param storage_location_id e.g.
#' @param key key
#'
#' @export
#'
new_object <- function(storage_location_id,
                       key) {

  post_data(table = "object",
            data = list(storage_location = storage_location_id),
            key)
}
