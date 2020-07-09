#' new_data_product
#'
#' @param name
#' @param version
#' @param object
#' @param namespace
#'
#' @export
#'
new_data_product <- function(name,
                             version,
                             object,
                             namespace) {

  object_url <- get_url("object", list(id = object))
  Namespace_url <- get_url("namespace", list(id = namespace))

  post_data(table = "data_product",
            data =  list(name = name,
                         version = version,
                         object = object_url,
                         namespace = Namespace_url),
            key)
}
