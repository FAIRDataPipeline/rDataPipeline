#' Post entry to code_run table
#'
#' Upload information to the \code{code_run} table in the data registry
#' 
#' @keywords internal
#'
#' @param run_date the date-time of the \code{code_run}
#' *e.g.* \code{Sys.time()} or "2010-07-11 12:15:00 BST"
#' @param description (optional) a \code{string} containing a free text
#' description of the \code{code_run}
#' @param code_repo_url (optional) a \code{string} specifying the URL of an
#' \code{object} associated with the \code{code_repo_release} that was run
#' @param model_config_url (optional) a \code{string} specifying the URL of an
#' \code{object} associated with the working config file used for the
#' \code{code_run}
#' @param submission_script_url (optional) a \code{string} specifying the URL
#' of an \code{object} associated with the submission script used for the
#' \code{code_run}
#' @param inputs_urls a \code{list} of \code{object_component} URLs referencing
#' \code{code_run} inputs
#' @param outputs_urls a \code{list} of \code{object_component} URLs referencing
#' \code{code_run} outputs
#' @param endpoint a \code{string} specifying the registry endpoint
#'
#' @family new functions
#'
new_code_run <- function(run_date,
                         description,
                         code_repo_url,
                         model_config_url,
                         submission_script_url,
                         inputs_urls = list(),
                         outputs_urls = list(),
                         endpoint = "http://127.0.0.1:8000/api/") {

  data <- list(run_date = run_date,
               inputs = inputs_urls,
               outputs = outputs_urls)

  if (!missing(description))
    data$description <- description

  if (!missing(code_repo_url))
    data$code_repo <- code_repo_url

  if (!missing(model_config_url))
    data$model_config <- model_config_url

  if (!missing(submission_script_url))
    data$submission_script <- submission_script_url

  post_data(table = "code_run",
            data = data,
            endpoint = endpoint)
}
