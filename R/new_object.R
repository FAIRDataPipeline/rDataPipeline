#' new_object
#'
#' @param id
#' @param store_id
#'
#' @export
#'
new_object <- function(id,
                       store_id) {

  store_url <- get_url("StorageLocation", list(id = store_id))

  post_data(table = "Object",
            data =  list(id = id,
                         store_id = store_url))
}
