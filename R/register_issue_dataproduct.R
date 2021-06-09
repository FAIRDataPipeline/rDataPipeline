#' register_issue_dataproduct
#'
#' @param handle handle
#' @param this_issue this_issue
#'
register_issue_dataproduct <- function(handle, this_issue) {

  this_id <- this_issue$index

  # Extract data product version

  if (grepl("\\{DATETIME\\}", this_issue$version)) {
    index_inputs <- which(this_id %in% handle$inputs$index)
    index_outputs <- which(this_id %in% handle$outputs$index)

    if (length(index_inputs) != 0) {
      issue_version <- handle$inputs$version[index_inputs]

    } else if (length(index_outputs) != 0) {
      issue_version <- handle$outputs$version[index_outputs]
    }

  } else {
    issue_version <- this_issue$version
  }

  tmp <- check_issue(this_issue$issue, this_issue$severity)
  issueId <- tmp$issueId
  current_objects <- tmp$current_objects
  current_components <- tmp$current_components

  # Which object component do we want to associate with the issue?
  namespace_uri <- get_url("namespace", list(name = this_issue$namespace))
  namespace_id <- extract_id(namespace_uri)
  dataProduct <- get_entry("data_product", list(name = this_issue$data_product,
                                            version = issue_version,
                                            namespace = namespace_id))
  assertthat::assert_that(length(dataProduct) == 1)
  object_uri <- dataProduct[[1]]$object
  object_id <- extract_id(object_uri)

  if (!is.na(this_issue$component)) {
    component_id <- get_url("object_component",
                                 list(name = this_issue$component,
                                      object = object_id))
    # Add this to the current list
    component_issues <- c(current_components, component_id)
    object_issues <- current_objects

  } else {
    # Add this to the current list
    component_issues <- current_components
    object_issues <- c(current_objects, object_uri)
  }

  # Upload issue to the data registry ---------------------------------------

  patch_data(url = issueId,
             data = list(severity = this_issue$severity,
                         description = this_issue$issue,
                         object_issues = object_issues,
                         component_issues = component_issues))
}
