#' initialise
#'
#' Reads in config.yaml file and generates new entry in code_run and returns id
#' and creates new submission_script in local registry and if necessary creates
#' a new code_repo entry in local registry.
#'
#' @export
#'
initialise <- function() {

  # Read config.yaml --------------------------------------------------------

  config_path <- Sys.getenv("wconfig")

  if (config_path == "")
    usethis::ui_stop(paste("Working", usethis::ui_value("config.yaml"),
                           "does not exist, please run `fdp pull` and try again"))

  yaml <- yaml::read_yaml(config_path)
  contents <- names(yaml)
  run_metadata <- yaml$run_metadata
  datastore_root <- yaml$run_metadata$default_data_store
  localstore <- run_metadata$default_data_store

  usethis::ui_done(paste("Read", usethis::ui_value("config.yaml")))

  # Record config.yaml location in data registry ----------------------------

  run_server()

  config_storageroot_id <- new_storage_root(name = "localstore",
                                            root = datastore_root,
                                            accessibility = 0)

  hash <- get_file_hash(config_path)
  config_location_id <- new_storage_location(path = config_path,
                                             hash = hash,
                                             storage_root_id = config_storageroot_id)
  config_object_id <- new_object(storage_location_id = config_location_id,
                                 description = "Local datastore")

  usethis::ui_done(paste("Record", usethis::ui_value("config.yaml"),
                         "in local registry"))

  # Get latest commit sha ---------------------------------------------------

  if ("local_repo" %in% names(run_metadata)) {
    check_local_repo(run_metadata$local_repo)
    sha <- git2r::sha(git2r::last_commit(run_metadata$local_repo))
  } else {
    sha <- get_github_hash(run_metadata$remote_repo)
  }

  usethis::ui_done(paste("Check local repository status"))

  # Get GitHub username/repository ------------------------------------------

  if ("remote_repo" %in% names(run_metadata)) {
    repo_name <- gsub("https://github.com/", remote_repo)

  } else if ("local_repo" %in% names(run_metadata)){
    repo_name <- git2r::remote_url(run_metadata$local_repo) %>%
      gsub("https://github.com/", "", .) %>%
      gsub(".git$", "", .)

  } else
    stop("No repo?")

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

  # Save submission script to data store ------------------------------------

  filename <- gsub("yaml", "sh", basename(config_path))
  path <- file.path(paste0(localstore, "scripts"), filename)
  submission_script_path <- store_script(run_metadata$script, path)

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

  fdp$new(yaml = yaml,
          model_config = config_object_id,
          submission_script = script_object_id)
}
