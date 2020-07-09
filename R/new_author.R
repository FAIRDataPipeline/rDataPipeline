#' new_author
#'
#' @param object
#' @param family_name
#' @param personal_name
#'
#' @export
#'
new_author <- function(object,
                       family_name,
                       personal_name) {

  object_url <- get_url("Object", list(id = object))

  post_data(table = "Author",
            data =  list(family_name = family_name,
                         personal_name = personal_name,
                         object = object_url),
            key)
}
