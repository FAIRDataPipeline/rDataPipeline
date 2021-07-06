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
  if (!file.exists(config)) usethis::ui_stop("{config} doesn't exist.")
  yaml <- yaml::read_yaml(config)
  contents <- names(yaml)
  run_metadata <- yaml$run_metadata

  filename <- basename(config)
  cli::cli_alert_info("Reading {.file {filename}} from data store")

  # Record config.yaml location in data registry ----------------------------

  datastore_root <- yaml$run_metadata$write_data_store
  config_storageroot_id <- new_storage_root(root = datastore_root,
                                            local = TRUE)

  config_hash <- get_file_hash(config)

  config_exists <- get_url("storage_location", list(hash = config_hash))

  if (is.null(config_exists)) {
    config_location_url <- new_storage_location(
      path = config,
      hash = config_hash,
      public = TRUE,
      storage_root_url = config_storageroot_id)

  } else {
    assertthat::assert_that(length(config_exists) == 1)
    config_location_url <- config_exists
  }

  config_filetype_exists <- get_url("file_type", list(extension = "yaml"))

  if (is.null(config_filetype_exists)) {
    config_filetype_url <- new_file_type(name = "yaml", extension = "yaml")
  } else {
    config_filetype_url <- config_filetype_exists
  }

  # authors_url <- get_url("user_author_org", list())

  config_object_url <- new_object(
    description = "Working config.yaml file location in local datastore",
    storage_location_url = config_location_url,
    # authors_urls = list(authors_url),
    file_type_url = config_filetype_url)

  cli::cli_alert_success("Writing {.file {config}} to local registry")

  # Record submission script location in data registry ----------------------

  script_storageroot_id <- config_storageroot_id

  script_hash <- get_file_hash(script)

  script_exists <- get_url("storage_location", list(hash = script_hash))

  if (is.null(script_exists)) {
    script_location_url <- new_storage_location(
      path = script,
      hash = script_hash,
      public = TRUE,
      storage_root_url = script_storageroot_id)

  } else {
    assertthat::assert_that(length(script_exists) == 1)
    script_location_url <- script_exists
  }

  script_filetype_exists <- get_url("file_type", list(extension = "sh"))

  if (is.null(script_filetype_exists)) {
    script_filetype_url <- new_file_type(name = "sh", extension = "sh")
  } else {
    script_filetype_url <- script_filetype_exists
  }

  script_object_url <- new_object(
    description = "Submission script location in local datastore",
    storage_location_url = script_location_url,
    # authors_urls = list(authors_url),
    file_type_url = script_filetype_url)

  cli::cli_alert_success("Writing {.file {script}} to local registry")

  # Record code repo location in data registry ------------------------------

  repo_storageroot_url <- new_storage_root(root = "https://github.com/",
                                           local = FALSE)

  sha <- yaml$run_metadata$latest_commit
  repo_name <- yaml$run_metadata$remote_repo

  coderepo_exists <- get_id("storage_location", list(hash = sha))

  if (is.null(coderepo_exists)) {
    coderepo_location_url <- new_storage_location(
      path = repo_name,
      hash = sha,
      public = TRUE,
      storage_root_url = repo_storageroot_url)

    coderepo_object_url <- new_object(
      description = "Analysis / processing script location",
      storage_location_url = coderepo_location_url)

  } else {
    assertthat::assert_that(length(coderepo_exists) == 1)
    coderepo_location_id <- coderepo_exists
    obj_exists <- get_url("object",
                          list(storage_location = coderepo_location_id))

    if (is.null(obj_exists)) {
      coderepo_object_url <- new_object(
        storage_location_url = coderepo_location_id,
        description = "Analysis / processing script location")

    } else {
      assertthat::assert_that(length(obj_exists) == 1)
      coderepo_object_url <- obj_exists
    }
  }

  cli::cli_alert_success("Writing {.file {repo_name}} to local registry")

  # Record the code run in the data registry --------------------------------

  coderun_url <- new_code_run(run_date = Sys.time(),
                              description = yaml$run_metadata$description,
                              code_repo_url = coderepo_object_url,
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
