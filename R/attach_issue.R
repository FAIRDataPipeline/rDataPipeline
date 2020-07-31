#' attach_issue_to_object
#'
#' @param data_product e.g. "records/SARS-CoV-2/scotland/cases_and_management"
#' @param description e.g. "Data dump caused a spike on the 15th of June"
#' @param severity e.g. 19
#' @param key key
#'
#' @export
#'
attach_issue_to_object <- function(data_product,
                                   description,
                                   severity,
                                   key) {

  # Does this issue already exist?
  if(check_exists("issue", list(description = description,
                                severity = severity))) {
    issueId <- get_url("issue", list(description = description,
                                     severity = severity))
    message("issue already exists")

  } else {
    # If not, create a new issue
    issueId <- new_issue(severity = severity,
                         description = description,
                         object_issues = list(),
                         component_issues = list(),
                         key = key)
  }

  # What objects / components is the issue currently associated with?
  tmp <- get_entry("issue", list(description = description))
  current_objects <- tmp$object_issues
  current_components <- tmp$component_issues


  # Find the data product object we want to attach an issue to
  objectId <- get_entry("data_product", list(name = data_product))$object

  # Find any object components we want to attach an issue to
  if(missing(object_components)) {
    data <- list(object = objectId)
    componentIds <- get_url("object_component", clean_query(data))

  } else {
    componentIds <- sapply(object_components, function(x)
      get_url("object_component", list(name = x))) %>% unname()
  }

  # Add these Ids to the existing lists
  if(!missing(objects))
    object_issues <- c(current_objects, objectId)

  if(!missing(object_components))
    component_issues <- c(current_components, componentIds)

  # Attach issue to object component in the data registry
  patch_data(url = issueId,
             data = list(severity,
                         description,
                         object_issues,
                         component_issues),
             key = key)
}



