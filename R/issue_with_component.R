#' issue_with_component
#'
#' @param issue issue
#' @param severity severity
#' @param data_product data_product
#' @param namespace namespace
#' @param component component
#' @param version version
#'
issue_with_component <- function(issue,
                                 severity,
                                 data_product,
                                 namespace,
                                 component,
                                 version) {

  tmp <- check_issue(issue, severity)
  issueId <- tmp$issueId
  current_objects <- tmp$current_objects
  current_components <- tmp$current_components

  # Which object component do we want to associate with the issue?
  namespaceId <- get_url("namespace", list(name = namespace))
  namespaceId <- extract_id(namespaceId)
  entries <- get_entry("data_product", list(name = data_product,
                                            version = version,
                                            namespace = namespaceId))
  objectId <- lapply(entries, function(x) x$object) %>% unlist() %>% unique()
  assertthat::assert_that(length(objectId) == 1)

  objectId <- extract_id(objectId)
  objectComponentId <- get_url("object_component", list(name = component,
                                                        object = objectId))

  # Add this to the current list
  object_issues <- current_objects
  component_issues <- c(current_components, objectComponentId)

  # Upload issue to the data registry ---------------------------------------

  message("Attaching issue to object component")
  patch_data(url = issueId,
             data = list(severity = severity,
                         description = issue,
                         object_issues = object_issues,
                         component_issues = component_issues))
}
