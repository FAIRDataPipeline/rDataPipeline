#' fdp_run
#'
#' @param path string
#' @param skip don't bother checking whether the repo is clean
#'
#' @export
#'
fdp_run <- function(path = "config.yaml", skip = FALSE) {

  # Read config.yaml --------------------------------------------------------

  if (file.exists(path)) {
    yaml <- yaml::read_yaml(path)
    usethis::ui_done(paste("Read", usethis::ui_value("config.yaml")))
  } else
    usethis::ui_stop(paste(usethis::ui_value(path), "does not exist"))

  # Extract local data store location
  localstore <- yaml$run_metadata$default_data_store
  if (grepl("/$", localstore))
    localstore <- gsub("/$", "", localstore)

  run_metadata <- yaml$run_metadata
  if (any("read" %in% names(yaml))) read <- yaml$read else read <- list()
  if (any("write" %in% names(yaml))) write <- yaml$write else write <- list()

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
            run_server()
            existing <- get_entry("external_object",
                                  list(doi_or_unique_name = x$unique_name,
                                       title = x$title))
            stop_server()
            version <- lapply(existing, function(z) z$version) %>%
              unlist() %>% max()
          } else {
            version <- x$version
          }

          read[[index]]$version <- version
        }
      }
    }
  }

  # Save working config.yaml in data store ----------------------------------

  # If coderun directory doesn't exist, create it
  configdir <- file.path(localstore, "coderun",
                         format(Sys.time(), "%Y%m%d-%H%M%S"))
  dir.create(configdir, recursive = TRUE)

  config_path <- file.path(configdir, "config.yaml")

  # Generate working config.yaml file
  working_yaml <- list(run_metadata = run_metadata,
                       read = read,
                       write = write)
  yaml::write_yaml(working_yaml, file = config_path)
  usethis::ui_done(paste("Save working", usethis::ui_value("config.yaml"),
                         "in data store"))

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

  # Save submission script to data store ------------------------------------

  submission_script_path <- gsub("config.yaml", "script.sh", config_path)
  cat(yaml$run_metadata$script, file = submission_script_path)
  usethis::ui_done(paste("Save", usethis::ui_value("script.sh"),
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

  usethis::ui_done(paste("Record", usethis::ui_value("script.sh"),
                         "in local registry"))

  # Save FDP_CONFIG_DIR in global environment ------------------------------

  Sys.setenv(FDP_CONFIG_DIR = configdir)

  usethis::ui_done(paste("Save working", usethis::ui_value("config.yaml"),
                         "path in global environment"))

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
    gsub("https://github.com/", "", .data) %>%
    gsub(".git$", "", .data)


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

  stop_server()

  usethis::ui_done(paste("Record", usethis::ui_value("script.sh"),
                         "location in local registry"))

}
