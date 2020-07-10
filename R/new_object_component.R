#' new_object_component
#'
#' @param name
#' @param object
#' @param key
#'
#' @export
#'
new_object_component <- function(name,
                                 object,
                                 key) {

  object_url <- get_url("object", list(id = object))

  post_data(table = "object_component",
            data = list(name = name,
                        object = object_url ),
            key)
}
