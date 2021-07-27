#' Post entry to issue table
#'
#' Upload information to the \code{issue} table in the data registry
#'
#' @param severity an \code{integer} specifying the severity of the \code{issue}
#' @param description a \code{string} containing a free text description of the
#' \code{issue}
#' @param component_issues a \code{list} of \code{object_component} URLs with
#' which the \code{issue} is associated; this can be an empty list
#' @param endpoint a \code{string} specifying the registry endpoint
#'
#' @family new functions
#'
new_issue <- function(severity,
                      description,
                      component_issues,
                      endpoint = "http://localhost:8000/api/") {

  post_data(table = "issue",
            data =  list(severity = severity,
                         description = description,
                         component_issues = component_issues),
            endpoint = endpoint)
}
