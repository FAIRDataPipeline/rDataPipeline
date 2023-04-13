#' register_issue_dataproduct
#' 
#' @keywords internal
#'
#' @param handle an object of class \code{fdp, R6} containing metadata required
#' by the Data Pipeline API
#' @param this_issue this_issue
#'
register_issue_dataproduct <- function(handle, this_issue) {

  endpoint <- handle$yaml$run_metadata$local_data_registry_url

  # Which object component do we want to associate with the issue?
  namespace_url <- get_url(table = "namespace",
                           query = list(name = this_issue$use_namespace),
                           endpoint = endpoint)
  namespace_id <- extract_id(url = namespace_url, endpoint = endpoint)
  data_product_entry <- get_entry(
    table = "data_product",
    query = list(name = this_issue$use_data_product,
                 version = this_issue$use_version,
                 namespace = namespace_id),
    endpoint = endpoint)

  assertthat::assert_that(length(data_product_entry) == 1)
  object_url <- data_product_entry[[1]]$object
  object_id <- extract_id(object_url, endpoint = endpoint)

  if (is.na(this_issue$use_component)) {
    component_url <- get_url(table = "object_component",
                             query = list(object = object_id,
                                          whole_object = TRUE),
                             endpoint = endpoint)

  } else {
    component_url <- get_url(table = "object_component",
                             query = list(name = this_issue$use_component,
                                          object = object_id),
                             endpoint = endpoint)
  }

  # Upload issue to the data registry ---------------------------------------

  new_issue(severity = this_issue$severity,
            description = this_issue$issue,
            component_issues = list(component_url),
            endpoint = endpoint)
}
