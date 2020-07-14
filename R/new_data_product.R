#' new_data_product
#'
#' @param name e.g.
#' @param version e.g.
#' @param object_id e.g.
#' @param namespace_id e.g.
#' @param key key
#' @export
#'
new_data_product <- function(name,
                             version,
                             object_id,
                             namespace_id,
                             key) {

  if(missing(namespace_id)) {
    post_data(table = "data_product",
              data =  list(name = name,
                           version = version,
                           object = object_id),
              key)

  } else {

    post_data(table = "data_product",
              data =  list(name = name,
                           version = version,
                           object = object_id,
                           namespace = namespace_id),
              key)
  }
}
