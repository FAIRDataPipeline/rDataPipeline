#' register_issue_script
#'
#' @param handle an object of class \code{fdp, R6} containing metadata required
#' by the Data Pipeline API
#' @param this_issue this_issue
#' @param type type
#'
register_issue_script <- function(handle,
                                  this_issue,
                                  type) {

  endpoint <- handle$yaml$run_metadata$local_data_registry_url

  if (type == "config") {
    object <- handle$model_config
  } else if (type == "script") {
    object <- handle$submission_script
  } else if (type == "repo") {
    object <- handle$code_repo
  } else {
    usethis::ui_stop("Unknown type")
  }

  object_id <- extract_id(object, endpoint = endpoint)

  component_url <- get_url(table = "object_component",
                           query = list(object = object_id,
                                        whole_object = TRUE),
                           endpoint = endpoint)

  # Upload issue to the data registry ---------------------------------------

  new_issue(severity = this_issue$severity,
            description = this_issue$issue,
            component_issues = list(component_url),
            endpoint = endpoint)
}
