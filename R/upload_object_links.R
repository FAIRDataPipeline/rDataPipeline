#' upload_object_links
#'
#' @param run_date e.g.
#' @param run_identifier e.g.
#' @param code_repo_id e.g.
#' @param submission_script_id e.g.
#' @param inputs e.g.
#' @param outputs e.g.
#' @param key key
#'
#' @export
#'
upload_object_links <- function(run_date,
                                run_identifier,
                                code_repo_id,
                                submission_script_id,
                                inputs = list(),
                                outputs = list(),
                                key = key) {

  script_codeRunId <- new_coderun(
    run_date,
    run_identifier = run_identifier,
    code_repo_id = code_repo_id,
    submission_script_id = submission_script_id,
    inputs = inputs,
    outputs = outputs,
    key = key)
}
