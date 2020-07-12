#' new_author
#'
#' @param family_name e.g.
#' @param personal_name e.g.
#' @param object_id e.g.
#' @param key key
#'
#' @export
#'
new_author <- function(family_name,
                       personal_name,
                       object_id,
                       key) {

  post_data(table = "author",
            data =  list(family_name = family_name,
                         personal_name = personal_name,
                         object = object_id),
            key)
}
