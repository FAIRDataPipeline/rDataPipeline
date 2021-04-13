#' initialise
#'
#' Reads in config.yaml file and generates new entry in code_run and returns id
#' and creates new submission_script in local registry and if necessary creates
#' a new code_repo entry in local registry.
#'
#' @param path Path to config.yaml file (optional)
#'
#' @export
#'
initialise <- function(path = ".") {
  # Read yaml
  yaml <- yaml::read_yaml(file.path(path, "config.yaml"))
  contents <- names(yaml)
  run_metadata <- yaml$run_metadata

  # Get latest commit sha
  if ("local_repo" %in% names(run_metadata)) {
    check_local_repo(run_metadata$local_repo)
    sha <- git2r::sha(git2r::last_commit(run_metadata$local_repo))
  } else {
    sha <- get_github_hash(run_metadata$remote_repo)
  }

  # Get GitHub username/repository
  if ("remote_repo" %in% names(run_metadata)) {
    repo_name <- gsub("https://github.com/", remote_repo)

  } else {
    repo_name <- git2r::remote_url(run_metadata$local_repo) %>%
      gsub("https://github.com/", "", .) %>%
      gsub(".git$", "", .)
  }

  run_server()

  # Submission script location
  repo_storageroot_id <- new_storage_root(name = "github",
                                          root = "https://github.com/",
                                          accessibility = 0)
  repo_location_id <- new_storage_location(path = repo_name,
                                           hash = sha,
                                           storage_root_id = repo_storageroot_id)

  repo_object_id <- new_object(storage_location_id = repo_location_id)

  # TODO: what about code repo version?

  # Save submission script to data store
  time <- format(Sys.time(), "%Y%m%d-%H%M%S")
  submission_script_path <- store_script(yaml, time)
  datastore_root <- yaml$run_metadata$default_data_store
  script_storageroot_id <- new_storage_root(name = "localstore",
                                            root = datastore_root,
                                            accessibility = 0)
  hash <- get_file_hash(paste0(datastore_root, submission_script_path))
  script_location_id <- new_storage_location(path = submission_script_path,
                                             hash = hash,
                                             storage_root_id = script_storageroot_id)
  script_object_id <- new_object(storage_location_id = script_location_id)

  # `fdp run`:
  # - read config.yaml and generate a working-config.yaml with specific
  #   version numbers and no aliases
  # - save working-config.yaml in local data store
  # - save path to working-config.yaml in global environment in $wconfig
  fdp_run(yaml, time)
  config_path <- Sys.getenv("wconfig")

  config_storageroot_id <- new_storage_root(name = "localstore",
                                            root = datastore_root,
                                            accessibility = 0)
  hash <- get_file_hash(paste0(datastore_root, config_path))
  config_location_id <- new_storage_location(path = config_path,
                                             hash = hash,
                                             storage_root_id = config_storageroot_id)
  config_object_id <- new_object(storage_location_id = config_location_id)

  coderun_id <- new_coderun(run_date = Sys.time(),
                            description = yaml$run_metadata$description,
                            # code_repo_id = "",
                            model_config = config_object_id,
                            submission_script_id = script_object_id,
                            inputs = list(),
                            outputs = list())

  stop_server()

  invisible(list(yaml = yaml,
                 coderun = coderun_id))
}
