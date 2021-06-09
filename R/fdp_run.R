#' fdp_run
#'
#' @param path string
#' @param skip don't bother checking whether the repo is clean
#'
#' @export
#'
fdp_run <- function(path = "config.yaml", skip = FALSE) {

  # Save names in data store
  config_file <- "config.yaml"

  # Read config.yaml --------------------------------------------------------

  if (file.exists(path)) {
    yaml <- yaml::read_yaml(path)
    cli::cli_alert_info("Reading {.file {config_file}}")
  } else
    usethis::ui_stop(paste(usethis::ui_value(path), "does not exist"))

  # Extract local data store location
  localstore <- yaml$run_metadata$default_data_store
  if (grepl("/$", localstore))
    localstore <- gsub("/$", "", localstore)

  run_metadata <- yaml$run_metadata
  if (any("read" %in% names(yaml))) read <- yaml$read else read <- list()

  # Generate working config.yaml --------------------------------------------

  # Check for presence of `register` key
  if (any("register" %in% names(yaml))) {

    register <- yaml$register

    # If names(register) is not null, then a single entry has been added that
    # is not in a list, so put it in a list
    if (!all(is.null(names(register))))
      register <- list(register)

    # Find `register:` entries. Then, since fdp pull has already registered
    # these entries, re-list them under `read:` in the working config.yaml file
    types <- c("external_object", "data_product", "object")
    for (x in register) {
      for (y in types) {
        entry <- x[[y]]
        if (!is.null(entry)) {
          index <- length(read) + 1
          read[[index]] <- list()
          read[[index]][y] <- entry
          read[[index]]$doi_or_unique_name <- x$unique_name
          read[[index]]$title <- x$title

          # YUCK!
          if (grepl("\\{DATETIME\\}", x$version)) {
            datetime <- gsub("-", "", Sys.Date())
            version <- gsub("\\{DATETIME\\}", datetime, x$version)

          } else {
            version <- x$version
          }

          read[[index]]$version <- version
        }
      }
    }
  }

  if (any("write" %in% names(yaml))) {

    write <- yaml$write

    # If names(write) is not null, then a single entry has been added that
    # is not in a list, so put it in a list
    if (!all(is.null(names(write))))
      write <- list(write)

    for (i in seq_along(write)) {
      this_write <- write[[i]]
      if ("version" %in% names(this_write)) {
        if (grepl("\\{DATETIME\\}", this_write$version)) {
          datetime <- gsub("-", "", Sys.Date())
          version <- gsub("\\{DATETIME\\}", datetime, x$version)
          write[[i]]$version <- version
        }
      }
    }

  } else {
    write <- list()
  }

  # Save working config.yaml in data store ----------------------------------

  # If coderun directory doesn't exist, create it
  configdir <- file.path(localstore, "coderun",
                         format(Sys.time(), "%Y%m%d-%H%M%S"))
  dir.create(configdir, recursive = TRUE)

  config_path <- file.path(configdir, config_file)

  # Generate working config.yaml file
  working_yaml <- list(run_metadata = run_metadata,
                       read = read,
                       write = write)
  yaml::write_yaml(working_yaml, file = config_path)

  cli::cli_alert_success("Writing working {.file {config_file}} to data store")

  # Record config.yaml location in data registry ----------------------------

  run_server()

  datastore_root <- yaml$run_metadata$default_data_store
  datastore_name <- paste("local datastore:", datastore_root)
  config_storageroot_id <- new_storage_root(name = datastore_name,
                                            root = datastore_root,
                                            accessibility = 0)

  config_hash <- get_file_hash(config_path)
  config_location_uri <- new_storage_location(
    path = config_path,
    hash = config_hash,
    storage_root_uri = config_storageroot_id)

  config_object_uri <- new_object(
    storage_location_uri = config_location_uri,
    description = "Working config.yaml file location in local datastore")

  cli::cli_alert_success("Writing {.file {config_file}} to local registry")

  # Write submission script to data store -----------------------------------

  script_file <- "script.sh"
  submission_script_path <- gsub(config_file, script_file, config_path)
  cat(yaml$run_metadata$script, file = submission_script_path)
  cli::cli_alert_success("Writing {.file {script_file}} to local data store")

  # Record submission script location in data registry ----------------------

  script_storageroot_id <- config_storageroot_id

  script_hash <- get_file_hash(submission_script_path)
  script_location_uri <- new_storage_location(
    path = submission_script_path,
    hash = script_hash,
    storage_root_uri = script_storageroot_id)

  script_object_uri <- new_object(
    storage_location_uri = script_location_uri,
    description = "Submission script location in local datastore")

  cli::cli_alert_success("Writing {.file {script_file}} to local registry")

  # Save FDP_CONFIG_DIR in global environment ------------------------------

  Sys.setenv(FDP_CONFIG_DIR = configdir)
  variable_name <- "FDP_CONFIG_DIR"
  cli::cli_alert_success(
    "Writing {.value {variable_name}} to global environment")

  # Get latest commit sha ---------------------------------------------------

  assertthat::assert_that("local_repo" %in% names(yaml$run_metadata))

  # Check local repo is clean
  if (!skip) check_local_repo(yaml$run_metadata$local_repo)

  # Get hash of latest commit
  sha <- git2r::sha(git2r::last_commit(yaml$run_metadata$local_repo))

  cli::cli_alert_info("Checking local repository status")

  # Get GitHub username/repository ------------------------------------------

  if ("remote_repo" %in% names(yaml$run_metadata))
    repo_name <- gsub("https://github.com/", yaml$run_metadata$remote_repo)

  else {
    repo_name <- git2r::remote_url(yaml$run_metadata$local_repo)
    repo_name <- gsub("https://github.com/", "", repo_name, fixed = TRUE)
    repo_name <- gsub(".git", "", repo_name, fixed = TRUE)
  }

  cli::cli_alert_info("Locating remote repository")

  # Record analysis / processing script location in data registry -----------

  repo_storageroot_uri <- new_storage_root(name = "github",
                                          root = "https://github.com/",
                                          accessibility = 0)
  repo_location_uri <- new_storage_location(path = repo_name,
                                           hash = sha,
                                           storage_root_uri = repo_storageroot_uri)

  repo_object_uri <- new_object(
    storage_location_uri = repo_location_uri,
    description = "Analysis / processing script location")

  cli::cli_alert_success("Writing processing script to local registry")

  stop_server()
}
