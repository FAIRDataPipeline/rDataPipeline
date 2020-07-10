#' new_object_component
#'
#' @param name
#' @param objectId
#' @param key
#'
#' @export
#'
new_object_component <- function(name,
                                 objectId,
                                 key) {

  post_data(table = "object_component",
            data = list(name = name,
                        object = objectId ),
            key)
}
