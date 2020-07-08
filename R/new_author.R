#' new_author
#'
#' @param object_id
#' @param family_name
#' @param personal_name
#'
#' @export
#'
new_author <- function(object_id,
                       family_name,
                       personal_name) {

  object_url <- get_url("Object", list(id = object_id))

  post_data(table = "Author",
            data =  list(object_id = object_url,
                         family_name = family_name,
                         personal_name = personal_name))
}
