#' attach_issue
#'
#' @export
#'
attach_issue <- function(object_uri,
                         object_component = "testing_location/date-cumulative",
                         severity,
                         description = "Data dump caused a spike on the 15th of June",
                         key) {

  if(!missing(object_component)) {
    # Find object component in the data registry
    objectComponentId <- get_url("object_component",
                                 list(name = object_component))

    # Create issue
    issueId <- new_issue(severity = severity,
                         description = description,
                         key = key)

    # Attach issue to object component in the data registry
    patch_data(url = objectComponentId,
               data = list(issues = list()),
               key = key)
  }


}

