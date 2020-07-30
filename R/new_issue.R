#' new_issue
#'
#' @param severity e.g.
#' @param description e.g.
#' @param object_issues e.g.
#' @param component_issues e.g.
#' @param key key
#'
#' @export
#'
new_issue <- function(severity,
                      description,
                      object_issues,
                      component_issues,
                      key) {

  post_data(table = "issue",
            data =  list(severity = severity,
                         description = description,
                         object_issues = object_issues,
                         component_issues = component_issues),
            key)
}
