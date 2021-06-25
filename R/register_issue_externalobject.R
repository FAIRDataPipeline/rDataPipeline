#' register_issue_externalobject
#'
#' @param description description
#' @param severity severity
#' @param external_object_doi external_object_doi
#' @param version version
#'
register_issue_externalobject <- function(description,
                                          severity,
                                          external_object_doi,
                                          version) {

  tmp <- check_issue(description, severity)
  issueId <- tmp$issueId
  current_objects <- tmp$current_objects
  current_components <- tmp$current_components

  # Which external object do we want to associate with the issue?
  entries <- get_entry("external_object",
                       list(doi_or_unique_name = external_object_doi,
                            version = version))
  objectId <- lapply(entries, function(x) x$object) %>% unlist() %>% unique()
  assertthat::assert_that(length(objectId) == 1)

  # Add this to the current list
  object_issues <- c(current_objects, objectId)
  component_issues <- current_components

  # Upload issue to the data registry ---------------------------------------

  message("Attaching issue to object component")
  patch_data(url = issueId,
             data = list(severity = severity,
                         description = description,
                         object_issues = object_issues,
                         component_issues = component_issues))
}
