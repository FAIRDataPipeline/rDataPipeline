#' new_object_component
#'
#' @param id
#' @param object_id
#' @param name
#'
#' @export
#'
new_object_component <- function(id,
                                 object_id,
                                 name) {

  object_url <- get_url("Object", list(id = object_id))

  post_data(table = "ObjectComponent",
            data = list(id = id,
                        object_id = object_url,
                        name = name))
}
