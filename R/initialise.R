#' Initialise code run
#'
#' Reads in a working config file, generates new Code Run entry, and returns
#' a handle containing various metadata.
#'
#' @param config a \code{string} specifying the location of the working
#' config file in the data store
#' @param script a \code{string} specifying the location of the submission
#' script in the data store
#'
#' @return Returns an object of class \code{fdp, R6} containing metadata
#' required by the Data Pipeline API
#'
#' @export
#'
initialise <- function(config, script) {

  if (!file.exists(config)) usethis::ui_stop("{config} doesn't exist.")
  if (!file.exists(script)) usethis::ui_stop("{script} doesn't exist.")

  # Read working config.yaml ------------------------------------------------

  yaml <- yaml::read_yaml(config)
  contents <- names(yaml)
  run_metadata <- yaml$run_metadata
  endpoint <- run_metadata$local_data_registry_url

  filename <- basename(config)
  cli::cli_alert_info("Reading {.file {filename}} from data store")

  # Record config.yaml location in data registry ----------------------------

  datastore_root <- yaml$run_metadata$write_data_store
  if (grepl("^/", datastore_root))
    datastore_root <- paste0("file://", datastore_root)

  config_storageroot_url <- new_storage_root(root = datastore_root,
                                             local = TRUE,
                                             endpoint = endpoint)
  config_storageroot_id <- extract_id(config_storageroot_url,
                                      endpoint = endpoint)

  config_hash <- get_file_hash(config)

  config_exists <- get_url(table = "storage_location",
                           query = list(hash = config_hash,
                                        public = TRUE,
                                        storage_root = config_storageroot_id),
                           endpoint = endpoint)

  if (is.null(config_exists)) {
    config_location_url <- new_storage_location(
      path = config,
      hash = config_hash,
      public = TRUE,
      storage_root_url = config_storageroot_url,
      endpoint = endpoint)

  } else {
    assertthat::assert_that(length(config_exists) == 1)
    config_location_url <- config_exists
  }

  config_filetype_exists <- get_url(table = "file_type",
                                    query = list(extension = "yaml"),
                                    endpoint = endpoint)

  if (is.null(config_filetype_exists)) {
    config_filetype_url <- new_file_type(name = "yaml",
                                         extension = "yaml",
                                         endpoint = endpoint)
  } else {
    config_filetype_url <- config_filetype_exists
  }

  author_url <- get_author_url(endpoint = endpoint)

  config_object_url <- new_object(
    description = "Working config.yaml file location in local datastore",
    storage_location_url = config_location_url,
    authors_url = list(author_url),
    file_type_url = config_filetype_url,
    endpoint = endpoint)

  cli::cli_alert_success("Writing {.file {config}} to local registry")

  # Record submission script location in data registry ----------------------

  script_storageroot_url <- config_storageroot_url
  script_storageroot_id <- config_storageroot_id

  script_hash <- get_file_hash(script)

  script_exists <- get_url(table = "storage_location",
                           query = list(hash = script_hash,
                                        public = TRUE,
                                        storage_root = script_storageroot_id),
                           endpoint = endpoint)

  if (is.null(script_exists)) {
    script_location_url <- new_storage_location(
      path = script,
      hash = script_hash,
      public = TRUE,
      storage_root_url = script_storageroot_url,
      endpoint = endpoint)

  } else {
    assertthat::assert_that(length(script_exists) == 1)
    script_location_url <- script_exists
  }

  script_filetype_exists <- get_url(table = "file_type",
                                    query = list(extension = "sh"),
                                    endpoint = endpoint)

  if (is.null(script_filetype_exists)) {
    script_filetype_url <- new_file_type(name = "sh",
                                         extension = "sh",
                                         endpoint = endpoint)
  } else {
    script_filetype_url <- script_filetype_exists
  }

  script_object_url <- new_object(
    description = "Submission script location in local datastore",
    storage_location_url = script_location_url,
    authors_url = list(author_url),
    file_type_url = script_filetype_url,
    endpoint = endpoint)

  cli::cli_alert_success("Writing {.file {script}} to local registry")

  # Record code repo location in data registry ------------------------------

  repo_name <- yaml$run_metadata$remote_repo

  if (is.null(repo_name)) {

    coderepo_object_url <- NA

  } else {

    repo_root <- gsub("([a-z]*://[a-z]*.[a-z]*/).*", "\\1", repo_name)

    repo_storageroot_url <- new_storage_root(root = repo_root,
                                             local = FALSE,
                                             endpoint = endpoint)
    repo_storageroot_id <- extract_id(repo_storageroot_url, endpoint = endpoint)

    sha <- yaml$run_metadata$latest_commit
    repo_name <- gsub(repo_root, "", repo_name)

    coderepo_exists <- get_url(table = "storage_location",
                               query = list(hash = sha,
                                            public = TRUE,
                                            storage_root = repo_storageroot_id),
                               endpoint = endpoint)

    repo_filetype_exists <- get_url(table = "file_type",
                                    query = list(extension = "git"),
                                    endpoint = endpoint)

    if (is.null(repo_filetype_exists)) {
      repo_filetype_url <- new_file_type(name = "git",
                                         extension = "git",
                                         endpoint = endpoint)
    } else {
      repo_filetype_url <- repo_filetype_exists
    }

    if (is.null(coderepo_exists)) {
      coderepo_location_url <- new_storage_location(
        path = repo_name,
        hash = sha,
        public = FALSE,
        storage_root_url = repo_storageroot_url,
        endpoint = endpoint)

      coderepo_object_url <- new_object(
        description = "Analysis / processing script location",
        storage_location_url = coderepo_location_url,
        authors_url = list(author_url),
        file_type_url = repo_filetype_url,
        endpoint = endpoint)

    } else {
      assertthat::assert_that(length(coderepo_exists) == 1)
      coderepo_location_url <- coderepo_exists
      coderepo_location_id <- extract_id(coderepo_location_url,
                                         endpoint = endpoint)
      obj_exists <- get_url(table = "object",
                            query = list(
                              storage_location = coderepo_location_id),
                            endpoint = endpoint)

      if (is.null(obj_exists)) {
        coderepo_object_url <- new_object(
          description = "Analysis / processing script location",
          storage_location_url = coderepo_location_url,
          authors_url = list(author_url),
          file_type_url = repo_filetype_url,
          endpoint = endpoint)

      } else {
        assertthat::assert_that(length(obj_exists) == 1)
        coderepo_object_url <- obj_exists
      }
    }
  }

  cli::cli_alert_success("Writing {.file {repo_name}} to local registry")

  # Record the code run in the data registry --------------------------------

  if (is.na(coderepo_object_url)) {
    coderun_url <- new_code_run(run_date = Sys.time(),
                                description = yaml$run_metadata$description,
                                model_config_url = config_object_url,
                                submission_script_url = script_object_url,
                                inputs_urls = list(),
                                outputs_urls = list(),
                                endpoint = endpoint)
  } else {
    coderun_url <- new_code_run(run_date = Sys.time(),
                                description = yaml$run_metadata$description,
                                code_repo_url = coderepo_object_url,
                                model_config_url = config_object_url,
                                submission_script_url = script_object_url,
                                inputs_urls = list(),
                                outputs_urls = list(),
                                endpoint = endpoint)
  }

  field <- "code_run"
  cli::cli_alert_success("Writing new {.field {field}} to local registry")

  # Write to handle
  fdp$new(yaml = yaml,
          fdp_config_dir = dirname(config),
          model_config = config_object_url,
          submission_script = script_object_url,
          code_repo = coderepo_object_url,
          code_run = coderun_url)
}
