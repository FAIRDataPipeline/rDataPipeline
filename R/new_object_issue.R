#' new_object_issue
#'
#' @param object_id
#' @param issue_id
#'
#' @export
#'
new_object_issue <- function(object_id,
                             issue_id) {

  object_url <- get_url("Object", list(id = object_id))
  issue_url <- get_url("Issue", list(id = issue_id))

  post_data(table = "Object",
            data =  list(object_id = object_url,
                         issue_id = issue_url))
}
