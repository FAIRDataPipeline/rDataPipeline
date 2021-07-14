#' fair_run
#'
#' @param path string
#' @param skip don't bother checking whether the repo is clean
#' @param endpoint a \code{string} specifying the registry endpoint
#'
#' @export
#'
fair_run <- function(path = "config.yaml",
                     endpoint = "http://localhost:8000/api/",
                     skip = FALSE) {

  if (grepl("localhost", endpoint)) run_server()

  # Save names in data store
  config_file <- "config.yaml"

  # Read config.yaml --------------------------------------------------------

  if (file.exists(path)) {
    yaml <- yaml::read_yaml(path)
    cli::cli_alert_info("Reading {.file {path}}")
  } else
    usethis::ui_stop(paste(usethis::ui_value(path), "does not exist"))

  # Extract local data store location
  localstore <- yaml$run_metadata$write_data_store
  localstore <- file.path(dirname(localstore), basename(localstore))

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
      read[[i]]$version <- NULL
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
    types <- c("external_object", "data_product")
    for (x in seq_along(register)) {
      for (y in types) {
        entry <- register[[x]][[y]]
        if (!is.null(entry)) {
          index <- length(read) + 1
          read[[index]] <- list()
          read[[index]]$data_product <- entry

          if ("public" %in% names(register[[x]])) {
            is_public <- register[[x]]$public
            if (tolower(is_public) == "true") {
              read[[index]]$use$public <- TRUE
            } else if (tolower(is_public) == "false") {
              read[[index]]$use$public <- FALSE
            } else {
              stop("Unknown value in public field")
            }
          } else {
            read[[index]]$use$public <- TRUE
          }

          if (grepl("\\$\\{\\{CLI.DATE\\}\\}", register[[x]]$version)) {
            datetime <- format(Sys.Date(), "%Y%m%d")
            version <- gsub("\\$\\{\\{CLI.DATE\\}\\}", datetime,
                            register[[x]]$version)
          } else {
            version <- register[[x]]$version
          }

          read[[index]]$use$version <- version
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

    # Check data_products are unique

    test <- lapply(write, function(x) x$data_product) %>%
      unlist() %>% duplicated() %>% any()
    if (test)
      usethis::ui_stop(paste("write block contains multiple",
                             usethis::ui_field("data_product"),
                             "entries with the same name"))

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

      write_namespace_url <- new_namespace(name = write_namespace,
                                           full_name = write_namespace,
                                           endpoint = endpoint)
      write_namespace_id <- extract_id(write_namespace_url)

      # Get public flag
      if ("public" %in% names(this_write)) {
        write_public <- this_write$public
      } else {
        write_public <- "true"
      }

      if (tolower(write_public) == "true") {
        write_public <- TRUE
      } else if (tolower(write_public) == "false") {
        write_public <- FALSE
      } else {
        stop ("public value not recognised")
      }
      write[[i]]$use$public <- write_public

      # Get version
      if ("version" %in% names(this_write)) { # before `use:` block
        write_version <- resolve_version(version = this_write$version,
                                         data_product = write_dataproduct,
                                         namespace_id = write_namespace_id)
        write[[i]]$use$version <- write_version # version should be here
        write[[i]]$version <- NULL

      } else if ("version" %in% names(alias)) { # in `use:` block
        write_version <- resolve_version(version = alias$version,
                                         data_product = write_dataproduct,
                                         namespace_id = write_namespace_id)
        write[[i]]$use$version <- write_version # version should be here

      } else { # version missing

        entries <- get_entry("data_product",
                             list(name = write_dataproduct,
                                  namespace = write_namespace_id),
                             endpoint = endpoint)

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
                                     namespace = write_namespace_id),
                                endpoint = endpoint)

      if (!is.null(check_exists))
        usethis::ui_stop("A data product with the same name ({write_dataproduct}), version ({write_version}), and namespace ({write_namespace}) already exists")
    }

  } else {
    write <- list()
  }

  # Get latest commit sha ---------------------------------------------------

  if (!skip) {
    assertthat::assert_that("local_repo" %in% names(yaml$run_metadata))

    # Check local repo is clean
    local_repo <- yaml$run_metadata$local_repo
    check_local_repo(local_repo)

    # Get hash of latest commit
    if (!dir.exists(local_repo))
      usethis::ui_stop("Directory, {local_repo}, does not exist")
    if (!git2r::in_repository(local_repo))
      usethis::ui_stop("Directory, {local_repo}, is not a git repository")

    sha <- git2r::sha(git2r::last_commit(local_repo))
    run_metadata$latest_commit <- sha

    cli::cli_alert_info("Checking local repository status")

  } else {
    random_hash <- openssl::sha1(as.character(Sys.time()))
    run_metadata$latest_commit <- random_hash
  }

  # Get GitHub username/repository ------------------------------------------

  if (!skip) {
    if ("remote_repo" %in% names(yaml$run_metadata)) {
      repo_name <- gsub("https://github.com/", "", yaml$run_metadata$remote_repo)
    } else {
      repo_name <- git2r::remote_url(yaml$run_metadata$local_repo)
      if (length(repo_name) > 1) stop("Add remote_repo field")

      repo_name <- gsub("https://github.com/", "", repo_name, fixed = TRUE)
      repo_name <- gsub(".git", "", repo_name, fixed = TRUE)
    }

    run_metadata$remote_repo <- repo_name
    cli::cli_alert_info("Locating remote repository")

  } else {
    fake_remote_repo <- "https://github.com/fake_org/fake_repo"
    run_metadata$remote_repo <- fake_remote_repo
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

  # Write submission script to data store -----------------------------------

  script_file <- "script.sh"
  submission_script_path <- gsub(config_file, script_file, config_path)
  cat(yaml$run_metadata$script, file = submission_script_path)
  cli::cli_alert_success("Writing {.file {script_file}} to local data store")

  # Save FDP_CONFIG_DIR in global environment ------------------------------

  Sys.setenv(FDP_CONFIG_DIR = configdir)
  variable_name <- "FDP_CONFIG_DIR"
  cli::cli_alert_success(
    "Writing {.file {variable_name}} to global environment")
}
