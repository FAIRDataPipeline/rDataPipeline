#' new_object_component
#'
#' @param name e.g.
#' @param object_id e.g.
#' @param key key
#'
#' @export
#'
new_object_component <- function(name,
                                 object_id,
                                 key) {

  post_data(table = "object_component",
            data = list(name = name,
                        object = object_id ),
            key)
}
