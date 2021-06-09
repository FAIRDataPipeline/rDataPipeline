#' Post entry to code_run table
#'
#' Upload information to the \code{coderun} table in the data registry
#'
#' @param run_date the date-time of the code run *e.g.* \code{Sys.time()} or
#' "2010-07-11 12:15:00 BST"
#' @param description (optional) a \code{string} containing a free text
#' description of the \code{code_run} *e.g.* "Script run to upload and process
#' scottish coronavirus-covid-19-management-information"
#' @param code_repo_uri (optional)
#' @param model_config_uri (optional) a \code{string} specifying the URI of an
#' \code{object} associated with the working config file
#' @param submission_script_uri a \code{string} specifying the URI of an
#' \code{object} associated with the submission script file
#' @param inputs a \code{list} of input component URIs
#' @param outputs a \code{list} of output component URIs
#'
#' @family new functions
#'
#' @export
#'
new_coderun <- function(run_date,
                        description = "",
                        code_repo_uri = "",
                        model_config_uri = "",
                        submission_script_uri = "",
                        inputs = list(),
                        outputs = list()) {

  post_data(table = "code_run",
            data = list(run_date = run_date,
                        description = description,
                        code_repo = code_repo_uri,
                        model_config = model_config_uri,
                        submission_script = submission_script_uri,
                        inputs = inputs,
                        outputs = outputs))
}
