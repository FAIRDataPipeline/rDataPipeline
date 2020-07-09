#' new_object_component
#'
#' @param name
#' @param object
#'
#' @export
#'
new_object_component <- function(name,
                                 object) {

  object_url <- get_url("object", list(id = object))

  post_data(table = "object_component",
            data = list(name = name,
                        object = object_url ),
            key)
}
