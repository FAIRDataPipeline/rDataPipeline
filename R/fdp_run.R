#' fdp_run
#'
#' @param path string
#' @param skip don't bother checking whether the repo is clean
#'
#' @export
#'
fdp_run <- function(path = "config.yaml", skip = FALSE) {

  run_server()

  # Save names in data store
  config_file <- "config.yaml"

  # Read config.yaml --------------------------------------------------------

  if (file.exists(path)) {
    yaml <- yaml::read_yaml(path)
    cli::cli_alert_info("Reading {.file {config_file}}")
  } else
    usethis::ui_stop(paste(usethis::ui_value(path), "does not exist"))

  # Extract local data store location
  localstore <- yaml$run_metadata$write_data_store
  if (grepl("/$", localstore))
    localstore <- gsub("/$", "", localstore)

  run_metadata <- yaml$run_metadata

  # Generate working config.yaml --------------------------------------------

  # Check for presence of `read` section
  if (any("read" %in% names(yaml))) {

    read <- yaml$read

    # If names(read) is not null, then a single entry has been added that
    # is not in a list, so put it in a list
    if (!all(is.null(names(read))))
      read <- list(read)

    # Ensure version number is present
    for (i in seq_along(read)) {
      read_version <- fdp_resolve_read(read[[i]], yaml)
      read[[i]]$use$version <- read_version # version should be here
    }

  } else {
    read <- list()
  }

  # Check for presence of `register` section
  if (any("register" %in% names(yaml))) {

    register <- yaml$register

    # If names(register) is not null, then a single entry has been added that
    # is not in a list, so put it in a list
    if (!all(is.null(names(register))))
      register <- list(register)

    # Find `register:` entries. Then, since fdp pull has already registered
    # these entries, re-list them under `read:` in the working config.yaml file
    types <- c("external_object", "data_product", "object")
    for (x in seq_along(register)) {
      for (y in types) {
        entry <- register[[x]][[y]]
        if (!is.null(entry)) {
          index <- length(read) + 1
          read[[index]] <- list()
          read[[index]][y] <- entry
          read[[index]]$doi_or_unique_name <- register[[x]]$unique_name
          read[[index]]$title <- register[[x]]$title

          # YUCK!
          if (grepl("\\$\\{\\{DATETIME\\}\\}", register[[x]]$version)) {
            datetime <- gsub("-", "", Sys.Date())
            version <- gsub("\\$\\{\\{DATETIME\\}\\}", datetime,
                            register[[x]]$version)

          } else {
            version <- register[[x]]$version
          }

          read[[index]]$version <- version
        }
      }
    }
  }

  # Check for presence of `write` section
  if (any("write" %in% names(yaml))) {

    write <- yaml$write

    # If names(write) is not null, then a single entry has been added that
    # is not in a list, so put it in a list
    if (!all(is.null(names(write))))
      write <- list(write)

    for (i in seq_along(write)) {
      this_write <- write[[i]]

      # Get alias
      if ("use" %in% names(this_write)) {
        alias <- this_write$use
      } else {
        alias <- list()
      }

      # Get data product
      if ("data_product" %in% names(alias)) {
        write_dataproduct <- alias$data_product
      } else {
        write_dataproduct <- this_write$data_product
      }

      # Get namespace
      if ("namespace" %in% names(alias)) {
        write_namespace <- alias$namespace
      } else {
        write_namespace <- yaml$run_metadata$default_output_namespace
      }

      write_namespace_url <- new_namespace(write_namespace)
      write_namespace_id <- extract_id(write_namespace_url)

      # Get version
      if ("version" %in% names(this_write)) { # version before `use:` block
        write_version <- resolve_version(version = this_write$version,
                                         data_product = write_dataproduct,
                                         namespace_id = write_namespace_id)
        write[[i]]$version <- write_version # version should be here

      } else if ("version" %in% names(alias)) { # version in `use:` block
        write_version <- resolve_version(version = alias$version,
                                         data_product = write_dataproduct,
                                         namespace_id = write_namespace_id)
        write[[i]]$use$version <- write_version # version should be here

      } else { # version missing

        entries <- get_entry("data_product",
                             list(name = write_dataproduct,
                                  namespace = write_namespace_id))

        if (is.null(entries)) {
          write_version <- "0.0.1"

        } else {
          write_version <- lapply(entries, function(x) x$version) %>%
            unlist() %>%
            max()
          tmp <- semver::parse_version(write_version)
          patch <- tmp[[1]]$patch
          tmp[[1]]$patch <- as.integer(patch + 1)
          write_version <- as.character(tmp)
        }
        write[[i]]$version <- write_version # version should be here
      }

      # If a data product already exists with the same name, version, and
      # namespace, throw an error
      check_exists <- get_entry("data_product",
                                list(name = write_dataproduct,
                                     version = write_version,
                                     namespace = write_namespace_id))

      if (!is.null(check_exists))
        usethis::ui_stop("A data product with the same name ({write_dataproduct}), version ({write_version}), and namespace ({write_namespace}) already exists")
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

  datastore_root <- yaml$run_metadata$write_data_store
  datastore_name <- paste("local datastore:", datastore_root)
  config_storageroot_id <- new_storage_root(name = datastore_name,
                                            root = datastore_root,
                                            accessibility = 0)

  config_hash <- get_file_hash(config_path)
  config_location_url <- new_storage_location(
    path = config_path,
    hash = config_hash,
    storage_root_url = config_storageroot_id)

  config_object_url <- new_object(
    storage_location_url = config_location_url,
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
  script_location_url <- new_storage_location(
    path = submission_script_path,
    hash = script_hash,
    storage_root_url = script_storageroot_id)

  script_object_url <- new_object(
    storage_location_url = script_location_url,
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
  local_repo <- yaml$run_metadata$local_repo
  if (!skip) check_local_repo(local_repo)

  # Get hash of latest commit
  if (!dir.exists(local_repo))
    usethis::ui_stop("Directory, {local_repo}, does not exist")
  if (!git2r::in_repository(local_repo))
    usethis::ui_stop("Directory, {local_repo}, is not a git repository")

  sha <- git2r::sha(git2r::last_commit(local_repo))

  cli::cli_alert_info("Checking local repository status")

  # Get GitHub username/repository ------------------------------------------

  if ("remote_repo" %in% names(yaml$run_metadata)) {
    repo_name <- gsub("https://github.com/", yaml$run_metadata$remote_repo)
  } else {
    repo_name <- git2r::remote_url(yaml$run_metadata$local_repo)
    repo_name <- gsub("https://github.com/", "", repo_name, fixed = TRUE)
    repo_name <- gsub(".git", "", repo_name, fixed = TRUE)
  }

  cli::cli_alert_info("Locating remote repository")

  # Record analysis / processing script location in data registry -----------

  repo_storageroot_url <- new_storage_root(name = "github",
                                           root = "https://github.com/",
                                           accessibility = 0)
  repo_location_url <- new_storage_location(path = repo_name,
                                            hash = sha,
                                            storage_root_url = repo_storageroot_url)

  repo_object_url <- new_object(
    storage_location_url = repo_location_url,
    description = "Analysis / processing script location")

  cli::cli_alert_success("Writing processing script to local registry")
}
