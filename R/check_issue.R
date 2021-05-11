#' check_issue
#'
#' @param description issue description
#' @param severity issue severity
#'
check_issue <- function(description,
                        severity) {

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
                         component_issues = list())
  }

  # What does the issue look like?
  tmp <- get_entry("issue", list(description = description))[[1]]
  assertthat::assert_that(any(names(tmp) %in% "url"))

  # Which objects are currently associated with the issue?
  current_objects <- tmp$object_issues
  if(is.null(current_objects)) current_objects <- list()

  # Which object components are currently associated with the issue?
  current_components <- tmp$component_issues
  if(is.null(current_components)) current_components <- list()

  list(issueId = issueId,
       current_objects = current_objects,
       current_components = current_components)
}
