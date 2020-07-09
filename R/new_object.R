#' new_object
#'
#' @param storage_location
#'
#' @export
#'
new_object <- function(storage_location) {

  store_url <- get_url("storage_location", list(id = store_id))

  post_data(table = "object",
            data =  list(storage_location = store_url),
            key)
}
