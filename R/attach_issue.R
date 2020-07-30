#' attach_issue
#'
#' @export
#'
attach_issue <- function(data_product,
                         object_components,
                         severity,
                         description,
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


  # Find data product object
  objectId <- get_entry("data_product", list(name = data_product))$object

  # Find object components
  if(missing(object_components)) {
    data <- list(object = objectId)
    componentIds <- get_url("object_component", clean_query(data))

  } else {
    componentIds <- sapply(object_components, function(x)
      get_url("object_component", list(name = x))) %>% unname()
  }


  # What objects / components is the issue currently associated with?
  tmp <- get_entry("issue", list(description = description))
  current_objects <- tmp$object_issues
  current_components <- tmp$component_issues




  if(!missing(objects))
    object_issues <- c(current_objects, objects)


  if(!missing(object_components))
    component_issues <- c(current_components, object_components)

  # Attach issue to object component in the data registry
  patch_data(url = issueId,
             data = list(severity,
                         description,
                         object_issues,
                         component_issues),
             key = key)
}



