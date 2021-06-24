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

  run_server()

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

  # Generate working config.yaml --------------------------------------------

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


  # Check for presence of `read` section
  if (any("read" %in% names(yaml))) {

    read <- yaml$read

    # If names(read) is not null, then a single entry has been added that
    # is not in a list, so put it in a list
    if (!all(is.null(names(read))))
      read <- list(read)

    for (i in seq_along(read)) {
      this_read <- read[[i]]

      # Get alias
      if ("use" %in% names(this_read)) {
        alias <- this_read$use
      } else {
        alias <- list()
      }

      # Get data product
      if ("data_product" %in% names(alias)) {
        read_dataproduct <- alias$data_product
      } else {
        read_dataproduct <- this_read$data_product
      }

      # Get namespace
      if ("namespace" %in% names(alias)) {
        read_namespace <- alias$namespace
      } else {
        read_namespace <- yaml$run_metadata$default_input_namespace
      }

      read_namespace_url <- new_namespace(read_namespace)
      read_namespace_id <- extract_id(read_namespace_url)

      # Get version
      if ("version" %in% names(alias)) {
        read_version <- alias$version

      } else {
        entries <- get_entry("data_product",
                             list(name = read_dataproduct,
                                  namespace = read_namespace_id))
        if (is.null(entries)) {
          stop("Something went wrong")

        } else {
          read_version <- lapply(entries, function(x) x$version) %>%
            unlist() %>%
            max()
        }

        read[[i]]$use$version <- read_version
      }
    }

  } else {
    read <- list()
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

      # Get write version
      if ("use" %in% names(this_write)) {
        alias <- this_write$use
      } else {
        alias <- list()
      }

      # If version is listed in the `use:` block
      if ("version" %in% names(alias)) {
        version <- alias$version

        if (grepl("\\$\\{\\{PATCH\\}\\}", version)) {
          stop("Not written")

        } else if (grepl("\\$\\{\\{MINOR\\}\\}", version)) {
          stop("Not written")

        } else if (grepl("\\$\\{\\{MAJOR\\}\\}", version)) {
          stop("Not written")

        } else if (grepl("\\$\\{\\{DATETIME\\}\\}", alias$version)) {
          datetime <- gsub("-", "", Sys.Date())
          write_version <- gsub("\\$\\{\\{DATETIME\\}\\}", datetime,
                                alias$version)

        } else {
          write_version <- alias$version
        }

        # If version isn't listed in the `use:` block
      } else {
        entries <- get_entry("data_product",
                             list(name = data_product,
                                  namespace = write_namespace_id))

        if (is.null(entries)) {
          write_version <- "0.0.1"

        } else {
          write_version <- lapply(entries, function(x) x$version) %>%
            unlist() %>%
            max()
        }
      }
      write[[i]]$use$version <- write_version

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

  datastore_root <- yaml$run_metadata$default_data_store
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
  if (!skip) check_local_repo(yaml$run_metadata$local_repo)

  # Get hash of latest commit
  sha <- git2r::sha(git2r::last_commit(yaml$run_metadata$local_repo))

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

  stop_server()
}
