#' register_issue_dataproduct
#'
#' @param issue issue
#' @param severity severity
#' @param data_product data_product
#' @param namespace namespace
#' @param version version
#'
register_issue_dataproduct <- function(issue,
                                   severity,
                                   data_product,
                                   namespace,
                                   version) {

  tmp <- check_issue(issue, severity)
  issueId <- tmp$issueId
  current_objects <- tmp$current_objects
  current_components <- tmp$current_components

  # Which data product do we want to associate with the issue?
  namespaceId <- get_url("namespace", list(name = namespace))
  namespaceId <- clean_query(list(name = namespaceId))
  entries <- get_entry("data_product", list(name = data_product,
                                            version = version,
                                            namespace = namespaceId))
  objectId <- lapply(entries, function(x) x$object) %>% unlist() %>% unique()
  assertthat::assert_that(length(objectId) == 1)

  # Add this to the current list
  object_issues <- c(current_objects, objectId)
  component_issues <- current_components

  # Upload issue to the data registry ---------------------------------------

  message("Attaching issue to object component")
  patch_data(url = issueId,
             data = list(severity = severity,
                         description = issue,
                         object_issues = object_issues,
                         component_issues = component_issues))
}
