#' new_component_issue
#'
#' @param component_id
#' @param issue_id
#'
#' @export
#'
new_component_issue <- function(component_id,
                                issue_id) {

  component_url <- get_url("Object", list(id = component_id))
  issue_url <- get_url("Issue", list(id = issue_id))

  post_data(table = "Object",
            data =  list(component_id = component_url,
                         issue_id = issue_url))
}
