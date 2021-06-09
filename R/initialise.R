#' initialise
#'
#' Reads in config.yaml file and generates new entry in code_run and returns id
#' and creates new submission_script in local registry and if necessary creates
#' a new code_repo entry in local registry.
#'
#' @export
#'
initialise <- function() {

  # Read working config.yaml ------------------------------------------------

  config_path <- Sys.getenv("FDP_CONFIG_DIR")
  config_file <- "config.yaml"
  script_file <- "script.sh"

  if (config_path == "")
    cli::cli_alert_danger(
      "{.file {config_file}} is missing from data store, please try again")

  yaml <- yaml::read_yaml(file.path(config_path, "config.yaml"))
  contents <- names(yaml)
  run_metadata <- yaml$run_metadata
  datastore_root <- yaml$run_metadata$default_data_store
  localstore <- run_metadata$default_data_store

  cli::cli_alert_info("Reading {.file {config_file}} from data store")

  run_server()

  # Get working config.yaml object id
  full_path <- file.path(config_path, "config.yaml")
  config_location_id <- get_entry("storage_location", list(path = full_path))
  assertthat::assert_that(length(config_location_id) == 1)
  config_location_id <- clean_query(config_location_id[[1]]$url)
  config_object_id <- get_entry("object",
                                list(storage_location = config_location_id))
  assertthat::assert_that(length(config_object_id) == 1)
  config_object_id <- config_object_id[[1]]$url

  cli::cli_alert_info(
    "Reading {.file {config_file}} metadata from local registry")

  # Get script object id
  full_path <- file.path(config_path, "script.sh")
  script_location_id <- get_entry("storage_location", list(path = full_path))
  assertthat::assert_that(length(script_location_id) == 1)
  script_location_id <- clean_query(script_location_id[[1]]$url)
  script_object_id <- get_entry("object",
                                list(storage_location = script_location_id))
  assertthat::assert_that(length(script_object_id) == 1)
  script_object_id <- script_object_id[[1]]$url

  cli::cli_alert_info(
    "Reading {.file {script_file}} metadata from local registry")

  # record the code run in the data registry --------------------------------

  coderun_id <- new_coderun(run_date = Sys.time(),
                            description = yaml$run_metadata$description,
                            # code_repo_id = "",
                            model_config = config_object_id,
                            submission_script_id = script_object_id,
                            inputs = list(),
                            outputs = list())

  field <- "code_run"
  cli::cli_alert_success("Writing new {.field {field}} to local registry")

  stop_server()

  # Write to handle
  fdp$new(yaml = yaml,
          model_config = config_object_id,
          submission_script = script_object_id,
          code_run = coderun_id)
}
