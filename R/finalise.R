#' finalise
#'
#' Push metadata to registry
#'
#' @param handle \code{fdp} object
#' @param endpoint endpoint
#'
#' @export
#'
finalise <- function(handle, endpoint) {

  # record data product metadata in the data registry --------

  # Add local data store root to the data registry

  datastore <- handle$yaml$run_metadata$write_data_store
  datastore_root_url <- new_storage_root(root = datastore,
                                         local = TRUE,
                                         endpoint = endpoint)
  datastore_root_id <- extract_id(datastore_root_url)

  if (!is.null(handle$outputs)) {

    # rename the data product as {hash}.h5 -----------------------------------

    data_products <- unique(handle$outputs$use_data_product)

    for (i in seq_along(data_products)) {

      index_row <- which(handle$outputs$use_data_product == data_products[i])
      this_write <- handle$outputs[index_row, ]
      write_data_product <- this_write$data_product
      write_use_data_product <- this_write$use_data_product
      write_namespace <- unique(this_write$use_namespace)
      write_version <- unique(this_write$use_version)
      write_namespace_url <- new_namespace(name = write_namespace,
                                           full_name = write_namespace,
                                           endpoint = endpoint)
      path <- unique(this_write$path)
      public <- this_write$public

      if (grepl("\\$\\{\\{DPAPI.RUN_ID\\}\\}", write_use_data_product)) {
        this_coderun <- get_entity(handle$code_run)
        uuid <- this_coderun$uuid

        write_use_data_product <- gsub("\\$\\{\\{DPAPI.RUN_ID\\}\\}",
                                       uuid,
                                       write_use_data_product)
      }

      # Get data product description (from config.yaml)
      index_dp <- which(unlist(lapply(handle$yaml$write, function(x)
        write_data_product == x$data_product)))
      description <- handle$yaml$write[[index_dp]]$description

      if (file.exists(path)) {
        hash <- get_file_hash(path)
      } else {
        usethis::ui_stop("File is missing")
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
        new_storage_location <- gsub(datastore, "", new_path)

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
        new_path <- replacement_path
      }

      extension <- strsplit(new_path, "\\.")[[1]][2]

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

      object_url <- new_object(storage_location_url = storage_location_url,
                               description = description,
                               file_type_url = file_type_url,
                               endpoint = endpoint)

      # Register data product in local registry
      data_product_url <- new_data_product(name = write_use_data_product,
                                           version = write_version,
                                           object_url = object_url,
                                           namespace_url = write_namespace_url,
                                           endpoint = endpoint)

      usethis::ui_done(paste("Writing", usethis::ui_value(write_use_data_product),
                             "to local registry"))

      # Update handle
      handle$finalise_output_hash(use_data_product = write_use_data_product,
                                  use_namespace = write_namespace,
                                  use_version = write_version,
                                  hash = hash,
                                  new_path = new_path,
                                  data_product_url = object_url)
    }

    # Register entries in local registry

    for (j in seq_len(nrow(handle$outputs))) {

      # Get metadata
      this_write <- handle$outputs[j, ]
      write_use_data_product <- this_write$use_data_product
      write_component <- this_write$use_component
      write_version <- this_write$use_version
      object_url <- this_write$data_product_url
      object_id <- extract_id(object_url)

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
      }

      # Update handle
      handle$finalise_output_url(use_data_product = write_use_data_product,
                                 use_component = write_component,
                                 component_url = component_url)

      usethis::ui_done(paste("Writing", usethis::ui_value(write_component),
                             usethis::ui_field("component"),
                             "to local registry"))
    }
  }

  issues <- handle$issues

  if (!is.null(issues)) {

    # Attach issues to components ---------------------------------------------

    component_issues <- handle$issues %>%
      dplyr::filter(!is.na(.data$use_component))

    if (nrow(component_issues) != 0) {
      for (k in seq_len(nrow(component_issues))) {

        this_issue <- component_issues[k, ]
        register_issue_dataproduct(handle = handle,
                                   this_issue = this_issue,
                                   endpoint = endpoint)

        usethis::ui_done(paste("Writing",
                               usethis::ui_value(this_issue$use_component),
                               usethis::ui_field("issue"),
                               "to local registry"))
      }
    }

    # Attach issues to data product

    dataproduct_issues <- handle$issues %>%
      dplyr::filter(is.na(.data$use_component))

    if (nrow(dataproduct_issues) != 0) {
      for (k in seq_len(nrow(dataproduct_issues))) {

        this_issue <- dataproduct_issues[k, ]
        register_issue_dataproduct(handle = handle,
                                   this_issue = this_issue,
                                   endpoint = endpoint)

        usethis::ui_done(paste("Writing",
                               usethis::ui_value(this_issue$use_data_product),
                               usethis::ui_field("issue"),
                               "to local registry"))
      }
    }

  }

  # record the code run in the data registry --------------------------------
  patch_data(url = handle$code_run,
             data = list(inputs = as.list(handle$inputs$component_url),
                         outputs = as.list(handle$outputs$component_url)))
}
