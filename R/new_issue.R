#' new_issue
#'
#' Upload information to the `issue` table in the data registry
#'
#' @param severity an `integer` specifying the severity of the `issue` *e.g.* 16
#' @param description a `string` containing a free text description of the
#' `issue` *e.g.* "Data dump caused a spike on the 15th of June"
#' @param object_issues a `list` of `object` URLs with which the `issue` is
#' associated *e.g.* list("https://data.scrc.uk/api/object/152/")
#' @param component_issues a `list` of `object_component` URLs with which the
#' `issue` is associated
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
