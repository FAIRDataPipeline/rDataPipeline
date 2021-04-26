#' fdp_run
#'
#' @param path string
#'
#' @export
#'
fdp_run <- function(path = "config.yaml") {

  # Read config.yaml --------------------------------------------------------

  if (file.exists(path)) {
    yaml <- yaml::read_yaml(path)
    usethis::ui_done("Read config.yaml")
  } else
    usethis::ui_stop(paste(usethis::ui_value(path), "does not exist"))

  # Extract local data store location
  localstore <- yaml$run_metadata$default_data_store
  if (grepl("/$", localstore))
    localstore <- gsub("/$", "", localstore)

  # Re-write config.yaml ----------------------------------------------------

  # Check for presence of `register` key
  if (any("register" %in% names(yaml))) {

    register <- yaml$register

    # If names(register) is null, then a single entry has been added that is
    # not in a list, so put it in a list
    if (!all(is.null(names(register))))
      register <- list(register)

    # Find entry to register
    types <- c("external_object", "data_product", "object")
    entries <- lapply(register, function(x)
      lapply(types, function(y) x[[y]]) %>% unlist())







  }

  # Save working config.yaml in data store ----------------------------------

  # If coderun directory doesn't exist, create it
  configdir <- file.path(localstore, "coderun",
                         format(Sys.time(), "%Y%m%d-%H%M%S"))
  if (!file.exists(configdir))
    dir.create(configdir)

  config_path <- file.path(configdir, "config.yaml")
  yaml::write_yaml(yaml, file = config_path)
  usethis::ui_done("Save working config.yaml in data store")

  # Record config.yaml location in data registry ----------------------------

  run_server()

  datastore_root <- yaml$run_metadata$default_data_store
  config_storageroot_id <- new_storage_root(name = "localstore",
                                            root = datastore_root,
                                            accessibility = 0)

  hash <- get_file_hash(config_path)
  config_location_id <- new_storage_location(
    path = config_path,
    hash = hash,
    storage_root_id = config_storageroot_id)
  config_object_id <- new_object(storage_location_id = config_location_id,
                                 description = "Local datastore")

  usethis::ui_done(paste("Record", usethis::ui_value("config.yaml"),
                         "in local registry"))

  stop_server()

  # Save FDP_CONFIG_DIR in global environment ------------------------------

  Sys.setenv(FDP_CONFIG_DIR = configdir)

  usethis::ui_done("Save working config.yaml path in global environment")

  # Save submission script to data store ------------------------------------

  submission_script_path <- gsub("config.yaml", "script.sh", config_path)
  cat(yaml$run_metadata$script, file = submission_script_path)
  usethis::ui_done(paste("Save", usethis::ui_value("submission script"),
                         "in local data store"))

  # Record submission script location in data registry ----------------------

  script_storageroot_id <- new_storage_root(name = "localstore",
                                            root = datastore_root,
                                            accessibility = 0)

  hash <- get_file_hash(submission_script_path)
  script_location_id <- new_storage_location(path = submission_script_path,
                                             hash = hash,
                                             storage_root_id = script_storageroot_id)
  script_object_id <- new_object(storage_location_id = script_location_id,
                                 description = "Submission script location")

  usethis::ui_done(paste("Record", usethis::ui_value("submission script"),
                         "in local registry"))

  stop_server()

  # Get latest commit sha ---------------------------------------------------

  assertthat::assert_that("local_repo" %in% names(yaml$run_metadata))

  # Check local repo is clean
  if (!skip) check_local_repo(yaml$run_metadata$local_repo)

  # Get hash of latest commit
  sha <- git2r::sha(git2r::last_commit(yaml$run_metadata$local_repo))

  usethis::ui_done(paste("Check local repository status"))

  # Get GitHub username/repository ------------------------------------------

  if ("remote_repo" %in% names(yaml$run_metadata))
    repo_name <- gsub("https://github.com/", yaml$run_metadata$remote_repo)

  else
    repo_name <- git2r::remote_url(yaml$run_metadata$local_repo) %>%
    gsub("https://github.com/", "", .) %>%
    gsub(".git$", "", .)


  usethis::ui_done(paste("Find remote repository"))

  # Record analysis / processing script location in data registry -----------

  repo_storageroot_id <- new_storage_root(name = "github",
                                          root = "https://github.com/",
                                          accessibility = 0)
  repo_location_id <- new_storage_location(path = repo_name,
                                           hash = sha,
                                           storage_root_id = repo_storageroot_id)

  repo_object_id <- new_object(storage_location_id = repo_location_id,
                               description = "Analysis / processing script location")

  usethis::ui_done(paste("Record", usethis::ui_value("analysis / processing script"),
                         "location in local registry"))

}
