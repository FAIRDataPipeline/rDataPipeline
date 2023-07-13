#' Finalise code run
#'
#' Finalise Code Run and push associated metadata to the local registry.
#'
#' If a Code Run does not read an input, write an output, or attach an issue,
#' then delete the Code Run entry when `delete_if_empty` is set to `TRUE`.
#'
#' If a data product has the same hash as a previous version, remove it from
#' the registry when `delete_if_duplicate` is set to `TRUE`.
#'
#' @param handle an object of class \code{fdp, R6} containing metadata required
#' by the Data Pipeline API
#' @param delete_if_empty (optional) default is `FALSE`; see Details
#' @param delete_if_duplicate (optional) default is `FALSE`; see Details
#'
#' @export
#'
finalise <- function(handle,
                     delete_if_empty = FALSE,
                     delete_if_duplicate = FALSE) {

  # If `delete_if_empty` is true and no input was read, output was written, or
  # issue was attached, then delete the CodeRun entry

  if (delete_if_empty) {
    if (is.null(handle$inputs) && is.null(handle$outputs) &&
        is.null(handle$issues)) {
      code_run_url <- handle$code_run
      key <- get_token()
      h <- c(Authorization = paste("token", key))
      result <- httr::DELETE(code_run_url,
                             httr::content_type("application/json"),
                             httr::add_headers(.headers = h),
                             verbose())
      return(invisible(NULL))
    }
  }

  # Record data product metadata in the data registry --------

  endpoint <- handle$yaml$run_metadata$local_data_registry_url

  # Add local data store root to the data registry

  datastore <- handle$yaml$run_metadata$write_data_store

  #datastore <- gsub  ( "\\\\",  "/", datastore)

  if (grepl("^/", datastore)) {
    appended_datastore <- paste0("file://", datastore)
  }
  else if (grepl("\\\\", datastore)) {
    appended_datastore <- paste0("file://", datastore)
  } else {
    appended_datastore <- datastore
  }

  datastore_root_url <- new_storage_root(root = appended_datastore,
                                         local = TRUE,
                                         endpoint = endpoint)
  datastore_root_id <- extract_id(datastore_root_url, endpoint = endpoint)

  if (!is.null(handle$outputs)) {

    # Rename file and register data product ---------------------------------

    data_products <- unique(handle$outputs$data_product)

    for (i in seq_along(data_products)) {

      index_row <- which(handle$outputs$data_product == data_products[i])

      this_write <- handle$outputs[index_row, ]
      write_use_data_product <- unique(this_write$use_data_product)
      write_dataproduct_description <- unique(
        this_write$data_product_decription)
      write_namespace <- unique(this_write$use_namespace)
      write_version <- unique(this_write$use_version)
      write_namespace_url <- new_namespace(name = write_namespace,
                                           endpoint = endpoint)
      path <- unique(this_write$path)
      public <- unique(this_write$public)

      if (grepl("\\$\\{\\{\\s*RUN_ID\\s*\\}\\}", write_use_data_product)) {
        this_coderun <- get_entity(handle$code_run)
        uuid <- this_coderun$uuid

        use_data_product_runid <- gsub("\\$\\{\\{\\s*RUN_ID\\s*\\}\\}",
                                       uuid,
                                       write_use_data_product)
      } else {
        use_data_product_runid <- write_use_data_product
      }

      # Get path
      if (file.exists(path)) {
        hash <- get_file_hash(path)
      } else {
        usethis::ui_stop("File is missing")
      }

      # If `delete_if_duplicate` is `TRUE`, check whether hash is the same as
      # previous versions of the data product
      if (delete_if_duplicate) {
        existing_dp <- get_entry("data_product",
                                 list(name = write_use_data_product),
                                 endpoint = endpoint)

        if (!is.null(existing_dp)) {
          existing_hash <- lapply(existing_dp, function(x) {
            existing_obj <- get_entity(x$object)
            existing_loc <- get_entity(existing_obj$storage_location)
            existing_loc$hash
          }) %>% unlist()

          if (hash %in% existing_hash) {
            # Remove file
            file.remove(path)

            # Recursively delete parent folders, if empty
            remove_empty_parents(path = path, root = datastore)

            # Remove from handle
            handle$finalise_output_hash(
              use_data_product = write_use_data_product,
              use_data_product_runid = use_data_product_runid,
              use_version = write_version,
              use_namespace = write_namespace,
              hash = hash,
              delete_if_duplicate = TRUE)

            next
          }
        }
      }

      # Does a file with the same hash already exist in the registry?
      storage_exists <- get_url("storage_location",
                                list(hash = hash,
                                     public = public,
                                     storage_root = datastore_root_id),
                                endpoint = endpoint)

      if (is.null(storage_exists)) {
        # Rename file
        tmp_filename <- basename(path)
        extension <- strsplit(tmp_filename, split = "\\.")[[1]][2]
        new_filename <- paste(hash, extension, sep = ".")
        new_path <- gsub(tmp_filename, new_filename, path)
        file.rename(path, new_path)
        new_storage_location <- gsub(datastore, "", new_path, fixed = TRUE)
        new_storage_location <- gsub(datastore, "", new_storage_location, fixed = TRUE)

        # Record file location in data registry
        storage_location_url <- new_storage_location(
          path = new_storage_location,
          hash = hash,
          public = public,
          storage_root_url = datastore_root_url,
          endpoint = endpoint)

      } else {
        # If a file already exists with the same hash, delete this one
        storage_location_url <- storage_exists
        file.remove(path)

        # Recursively delete parent folders, if empty
        remove_empty_parents(path = path, root = datastore)

        # Get metadata
        tmp <- get_entity(storage_location_url)
        existing_path <- tmp$path
        existing_root <- get_entity(tmp$storage_root)$root
        replacement_path <- paste0(existing_root, existing_path)
        if (grepl("^file://", replacement_path))
          replacement_path <- gsub("file://", "", replacement_path)
        new_path <- replacement_path
      }

      extension <- gsub(".*\\.(.*)$", "\\1", new_path)

      file_type_exists <- get_url(table = "file_type",
                                  query = list(extension = extension),
                                  endpoint = endpoint)

      if (is.null(file_type_exists)) {
        file_type_url <- new_file_type(name = extension,
                                       extension = extension,
                                       endpoint = endpoint)
      } else {
        file_type_url <- file_type_exists
      }

      author_url <- get_author_url(endpoint = endpoint)

      object_url <- new_object(description = write_dataproduct_description,
                               storage_location_url = storage_location_url,
                               authors_url = list(author_url),
                               file_type_url = file_type_url,
                               endpoint = endpoint)

      # Register data product in local registry
      data_product_url <- new_data_product(name = use_data_product_runid,
                                           version = write_version,
                                           object_url = object_url,
                                           namespace_url = write_namespace_url,
                                           endpoint = endpoint)

      usethis::ui_done(
        paste("Writing", usethis::ui_value(use_data_product_runid),
              "to local registry"))

      # Update handle
      handle$finalise_output_hash(
        use_data_product = write_use_data_product,
        use_data_product_runid = use_data_product_runid,
        use_version = write_version,
        use_namespace = write_namespace,
        hash = hash,
        new_path = new_path,
        data_product_url = object_url)
    }

    # Register components ---------------------------------------------------

    for (j in seq_len(nrow(handle$outputs))) {

      # Get metadata
      this_write <- handle$outputs[j, ]

      if (this_write$delete_if_duplicate) next

      write_use_data_product <- this_write$use_data_product
      write_component <- this_write$use_component
      write_version <- this_write$use_version
      write_namespace <- this_write$use_namespace
      object_url <- this_write$data_product_url
      object_id <- extract_id(object_url, endpoint = endpoint)

      # Register component in local registry
      if (is.na(write_component)) {
        component_url <- get_url(table = "object_component",
                                 query = list(object = object_id),
                                 endpoint = endpoint)
        assertthat::assert_that(length(component_url) == 1)

      } else {
        component_url <- new_object_component(name = write_component,
                                              object_url = object_url,
                                              endpoint = endpoint)

        usethis::ui_done(paste("Writing", usethis::ui_value(write_component),
                               usethis::ui_field("component"),
                               "to local registry"))
      }

      # Update handle
      handle$finalise_output_url(use_data_product = write_use_data_product,
                                 use_component = write_component,
                                 use_version = write_version,
                                 use_namespace = write_namespace,
                                 component_url = component_url)
    }
  }

  issues <- handle$issues

  if (!is.null(issues)) {

    # Attach issues to config -------------------------------------------------

    config_issues <- handle$issues %>%
      dplyr::filter(.data$type == "config")

    if (nrow(config_issues) != 0) {
      for (k in seq_len(nrow(config_issues))) {

        this_issue <- config_issues[k, ]
        register_issue_script(handle = handle,
                              this_issue = this_issue,
                              type = "config")

        usethis::ui_done(paste("Writing",
                               usethis::ui_value(this_issue$use_data_product),
                               usethis::ui_field("issue"),
                               "to local registry"))
      }
    }

    # Attach issues to script -------------------------------------------------

    script_issues <- handle$issues %>%
      dplyr::filter(.data$type == "script")

    if (nrow(script_issues) != 0) {
      for (k in seq_len(nrow(script_issues))) {

        this_issue <- script_issues[k, ]
        register_issue_script(handle = handle,
                              this_issue = this_issue,
                              type = "script")

        usethis::ui_done(paste("Writing",
                               usethis::ui_value(this_issue$use_data_product),
                               usethis::ui_field("issue"),
                               "to local registry"))
      }
    }

    # Attach issues to repo -------------------------------------------------

    repo_issues <- handle$issues %>%
      dplyr::filter(.data$type == "repo")

    if (nrow(repo_issues) != 0) {
      for (k in seq_len(nrow(repo_issues))) {

        this_issue <- repo_issues[k, ]
        register_issue_script(handle = handle,
                              this_issue = this_issue,
                              type = "repo")

        usethis::ui_done(paste("Writing",
                               usethis::ui_value(this_issue$use_data_product),
                               usethis::ui_field("issue"),
                               "to local registry"))
      }
    }

    # Attach issues to components ---------------------------------------------

    component_issues <- handle$issues %>%
      dplyr::filter(.data$type == "data",
                    !is.na(.data$use_component))

    if (nrow(component_issues) != 0) {
      for (k in seq_len(nrow(component_issues))) {

        this_issue <- component_issues[k, ]
        register_issue_dataproduct(handle = handle,
                                   this_issue = this_issue)

        usethis::ui_done(paste("Writing",
                               usethis::ui_value(this_issue$use_component),
                               usethis::ui_field("issue"),
                               "to local registry"))
      }
    }

    # Attach issues to data product -----------------------------------------

    dataproduct_issues <- handle$issues %>%
      dplyr::filter(.data$type == "data",
                    is.na(.data$use_component))

    if (nrow(dataproduct_issues) != 0) {
      for (k in seq_len(nrow(dataproduct_issues))) {

        this_issue <- dataproduct_issues[k, ]
        register_issue_dataproduct(handle = handle,
                                   this_issue = this_issue)

        usethis::ui_done(paste("Writing",
                               usethis::ui_value(this_issue$use_data_product),
                               usethis::ui_field("issue"),
                               "to local registry"))
      }
    }
  }

  # record the code run in the data registry --------------------------------

  if (all(is.na(handle$outputs$component_url))) {
    patch_data(url = handle$code_run,
               data = list(inputs = as.list(handle$inputs$component_url),
                           outputs = list()))
  } else {
    patch_data(url = handle$code_run,
               data = list(inputs = as.list(handle$inputs$component_url),
                           outputs = as.list(handle$outputs$component_url)))
  }

  code_run_uuid <- get_entity(handle$code_run)$uuid

  coderuns_txt <- file.path(handle$fdp_config_dir, paste0("coderuns.txt"))
  if (file.exists(coderuns_txt)) {
    cat(paste0("\n", code_run_uuid), file = coderuns_txt, append = TRUE)
  } else {
    cat(code_run_uuid, file = coderuns_txt)
  }

}
