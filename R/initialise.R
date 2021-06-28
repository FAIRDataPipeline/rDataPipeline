#' initialise
#'
#' Reads in a working config file and generates new Code Run entry.
#'
#' @param config a \code{string} specifying the location of the working
#' config file in the data store
#' @param script a \code{string} specifying the location of the submission
#' script in the data store
#'
#' @export
#'
initialise <- function(config, script) {

  # Read working config.yaml ------------------------------------------------
  yaml <- yaml::read_yaml(config)
  contents <- names(yaml)
  run_metadata <- yaml$run_metadata

  cli::cli_alert_info("Reading {.file {config}} from data store")

  # Get working config.yaml object url --------------------------------------

  config_location_url <- get_entry("storage_location",
                                   list(path = config))
  assertthat::assert_that(length(config_location_url) == 1)
  config_location_id <- extract_id(config_location_url[[1]]$url)

  config_object_url <- get_entry("object",
                                 list(storage_location = config_location_id))
  assertthat::assert_that(length(config_object_url) == 1)
  config_object_url <- config_object_url[[1]]$url

  cli::cli_alert_info(
    "Reading {.file {config}} metadata from local registry")

  # Get script object url ---------------------------------------------------

  script_location_url <- get_entry("storage_location",
                                   list(path = script))
  assertthat::assert_that(length(script_location_url) == 1)
  script_location_id <- extract_id(script_location_url[[1]]$url)

  script_object_url <- get_entry("object",
                                 list(storage_location = script_location_id))
  assertthat::assert_that(length(script_object_url) == 1)
  script_object_url <- script_object_url[[1]]$url

  cli::cli_alert_info(
    "Reading {.file {script}} metadata from local registry")

  # record the code run in the data registry --------------------------------

  coderun_url <- new_code_run(run_date = Sys.time(),
                              description = yaml$run_metadata$description,
                              # code_repo_id = "",
                              model_config_url = config_object_url,
                              submission_script_url = script_object_url,
                              inputs_urls = list(),
                              outputs_urls = list())

  field <- "code_run"
  cli::cli_alert_success("Writing new {.field {field}} to local registry")

  # Write to handle
  fdp$new(yaml = yaml,
          model_config = config_object_url,
          submission_script = script_object_url,
          code_run = coderun_url)
}
