#' Post entry to issue table
#'
#' Upload information to the \code{issue} table in the data registry
#'
#' @param severity an \code{integer} specifying the severity of the \code{issue}
#' @param description a \code{string} containing a free text description of the
#' \code{issue}
#' @param object_issues a \code{list} of \code{object} URLs with which the
#' \code{issue} is associated; this can be an empty list
#' @param component_issues a \code{list} of \code{object_component} URLs with
#' which the \code{issue} is associated; this can be an empty list
#'
#' @family new functions
#'
#' @export
#'
new_issue <- function(severity,
                      description,
                      object_issues,
                      component_issues) {

  post_data(table = "issue",
            data =  list(severity = severity,
                         description = description,
                         object_issues = object_issues,
                         component_issues = component_issues))
}
