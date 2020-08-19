#' new_coderun
#'
#' Upload information to the \code{coderun} table in the data registry
#'
#' @param run_date the date-time of the code run *e.g.* \code{Sys.time()} or
#' "2010-07-11 12:15:00 BST"
#' @param description (optional) a \code{string} containing a free text
#' description of the \code{code_run} *e.g.* "Script run to upload and process
#' scottish coronavirus-covid-19-management-information"
#' @param code_repo_id (optional) *e.g.* "https://data.scrc.uk/api/object/154/"
#' @param model_config (optional)
#' @param submission_script_id *e.g.* "https://data.scrc.uk/api/object/153/"
#' @param inputs *e.g.* list("https://data.scrc.uk/api/object_component/875/")
#' @param outputs *e.g.* list("https://data.scrc.uk/api/object_component/875/")
#' @param key API token from data.scrc.uk
#'
#' @export
#'
new_coderun <- function(run_date,
                        description = "",
                        code_repo_id = "",
                        model_config = "",
                        submission_script_id = "",
                        inputs = list(),
                        outputs = list(),
                        key) {

  post_data(table = "code_run",
            data =  list(run_date = run_date,
                         description = description,
                         code_repo = code_repo_id,
                         model_config = model_config,
                         submission_script = submission_script_id,
                         inputs = inputs,
                         outputs = outputs),
            key)
}
