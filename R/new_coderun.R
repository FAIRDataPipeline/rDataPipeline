#' new_coderun
#'
#' @param run_date e.g.
#' @param run_identifier e.g.
#' @param code_repo_id e.g.
#' @param model_config e.g.
#' @param submission_script_id e.g.
#' @param inputs e.g.
#' @param outputs e.g.
#' @param key key
#'
#' @export
#'
new_coderun <- function(run_date,
                        run_identifier,
                        code_repo_id = "",
                        model_config = "",
                        submission_script_id = "",
                        inputs = list(),
                        outputs = list(),
                        key) {

  post_data(table = "code_run",
            data =  list(run_date = run_date,
                         run_identifier = run_identifier,
                         code_repo = code_repo_id,
                         model_config = model_config,
                         submission_script = submission_script_id,
                         inputs = inputs,
                         outputs = inputs),
            key)
}
