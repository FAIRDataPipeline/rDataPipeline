#' new_data_product
#'
#' @param object_id
#' @param description
#' @param name
#' @param version
#'
#' @export
#'
new_data_product <- function(object_id,
                             prefix_id,
                             name,
                             version) {

  object_url <- get_url("Object", list(id = object_id))
  prefix_url <- get_url("Prefix", list(id = prefix_id))

  post_data(table = "DataProduct",
            data =  list(object_id = object_url,
                         prefix_id = prefix_url,
                         name = name,
                         version = version),
            key)
}
