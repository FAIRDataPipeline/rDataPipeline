#' Post object_links metadata to the data registry
#'
#' @param run_date e.g.
#' @param description e.g.
#' @param code_repo_id e.g.
#' @param submission_script_id e.g.
#' @param inputs e.g.
#' @param outputs e.g.
#'
#' @family upload functions
#'
#' @export
#'
upload_object_links <- function(run_date,
                                description,
                                code_repo_id,
                                submission_script_id,
                                inputs = list(),
                                outputs = list()) {

  script_codeRunId <- new_coderun(run_date = run_date,
                                  description = description,
                                  code_repo_id = code_repo_id,
                                  submission_script_id = submission_script_id,
                                  inputs = inputs,
                                  outputs = outputs)
}
