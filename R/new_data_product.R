#' new_data_product
#'
#' @param name
#' @param version
#' @param objectId
#' @param namespaceId
#' @param key
#' @export
#'
new_data_product <- function(name,
                             version,
                             objectId,
                             namespaceId,
                             key) {

  if(missing(namespaceId)) {
    post_data(table = "data_product",
              data =  list(name = name,
                           version = version,
                           object = objectId),
              key)

  } else {

    post_data(table = "data_product",
              data =  list(name = name,
                           version = version,
                           object = objectId,
                           namespace = namespaceId),
              key)
  }
}
