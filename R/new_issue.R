#' new_issue
#'
#' Upload information to the \code{issue} table in the data registry
#'
#' @param severity an \code{integer} specifying the severity of the \code{issue}
#' *e.g.* 16
#' @param description a \code{string} containing a free text description of the
#' `issue` *e.g.* "Data dump caused a spike on the 15th of June"
#' @param object_issues a \code{list} of \code{object} URLs with which the
#' \code{issue} is associated *e.g.* list("https://data.scrc.uk/api/object/152/")
#' @param component_issues a \code{list} of \code{object_component} URLs with
#' which the \code{issue} is associated
#' *e.g.* list("https://data.scrc.uk/api/object_component/902/")
#' @param key API token from data.scrc.uk
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
