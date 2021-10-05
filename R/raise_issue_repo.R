#' raise_issue_repo
#'
#' @param handle an object of class \code{fdp, R6} containing metadata required
#' by the Data Pipeline API
#' @param issue issue
#' @param severity severity
#'
#' @export
#'
raise_issue_repo<- function(handle,
                            issue,
                            severity) {

  handle$raise_issue(index = NA,
                     type = "repo",
                     use_data_product = NA,
                     use_component = NA,
                     use_version = NA,
                     use_namespace = NA,
                     issue = issue,
                     severity = severity)

  usethis::ui_done("Recording issues in handle")
}
