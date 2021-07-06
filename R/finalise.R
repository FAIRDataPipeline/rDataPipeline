#' finalise
#'
#' Push metadata to registry
#'
#' @param handle \code{fdp} object
#'
#' @export
#'
finalise <- function(handle) {

  # record data product metadata in the data registry --------

  # Add local data store root to the data registry

  datastore <- handle$yaml$run_metadata$write_data_store
  datastore_root_url <- new_storage_root(root = datastore,
                                         local = TRUE)
  datastore_root_id <- extract_id(datastore_root_url)

  if (!is.null(handle$outputs)) {

    # rename the data product as {hash}.h5 -----------------------------------

    data_products <- unique(handle$outputs$data_product)

    for (i in seq_along(data_products)) {

      index_row <- which(handle$outputs$data_product == data_products[i])
      this_write <- handle$outputs[index_row, ]
      this_namespace <- unique(this_write$use_namespace)
      this_version <- unique(this_write$use_version)

      # Get file path
      path <- unique(this_write$path)

      # Rename file
      if (file.exists(path)) {
        hash <- get_file_hash(path)
        tmp_filename <- basename(path)
        extension <- strsplit(tmp_filename, split = "\\.")[[1]][2]
        new_filename <- paste(hash, extension, sep = ".")
        new_path <- gsub(tmp_filename, new_filename, path)
        file.rename(path, new_path)
      } else {
        new_path <- this_write$path
        hash <- unique(this_write$hash)
        if (is.na(hash))
          usethis::ui_stop("Something is wrong")
      }

      # Update handle
      handle$finalise_output_hash(use_data_product = data_products[i],
                                  use_namespace = this_namespace,
                                  use_version = this_version,
                                  hash = hash,
                                  new_path = new_path)
    }

    # Register entries in local registry

    for (j in seq_len(nrow(handle$outputs))) {

      # Get metadata
      this_write <- handle$outputs[j, ]
      this_data_product <- this_write$data_product
      write_data_product <- this_write$use_data_product
      write_component <- this_write$use_component
      write_version <- this_write$use_version
      write_namespace <- this_write$use_namespace
      write_namespace_url <- new_namespace(name = write_namespace,
                                           full_name = write_namespace)

      # Check whether the data product has been registered on a previous loop
      # iteration
      if (!this_write$registered_data_product) {

        # Get data product description (from config.yaml)
        index_dp <- which(unlist(lapply(handle$yaml$write, function(x)
          this_data_product == x$data_product)))
        description <- handle$yaml$write[[index_dp]]$description

        # Record file location in data registry
        storage_location <- gsub(datastore, "", this_write$path)
        storage_exists <- get_url("storage_location",
                                  list(hash = hash,
                                       # public = public,
                                       storage_root = datastore_root_id))

        if (is.null(storage_exists)) {
          storage_location_url <- new_storage_location(
            path = storage_location,
            hash = hash,
            # public = public,
            storage_root_url = datastore_root_url)

        } else {
          storage_location_url <- storage_exists
        }

        object_url <- new_object(storage_location_url = storage_location_url,
                                 description = description)

        # Register data product in local registry
        dataproduct_url <- new_data_product(name = write_data_product,
                                            version = write_version,
                                            object_url = object_url,
                                            namespace_url = write_namespace_url)

        usethis::ui_done(paste("Writing", usethis::ui_value(this_data_product),
                               "to local registry"))

      } else {
        write_component <- this_write$use_component
        object_url <- this_write$data_product_url
      }

      # Register component in local registry
      if (is.na(write_component)) {
        component_url <- get_url("object_component",
                                 list(object = extract_id(object_url)))
        assertthat::assert_that(length(component_url) == 1)

      } else {
        component_url <- new_object_component(name = write_component,
                                              object_url = object_url)
      }

      # Update handle
      handle$finalise_output_url(use_data_product = write_data_product,
                                 use_component = write_component,
                                 data_product_url = object_url,
                                 component_url = component_url)

      usethis::ui_done(paste("Writing", usethis::ui_value(write_component),
                             usethis::ui_field("component"),
                             "to local registry"))
    }
  }





  # issues <- handle$issues
  #
  # if (!is.null(issues)) {
  #
  #   # Attach issues to components ---------------------------------------------
  #
  #   component_issues <- handle$issues %>%
  #     dplyr::filter(!is.na(component))
  #
  #   if (nrow(component_issues) != 0) {
  #     for (k in seq_len(nrow(component_issues))) {
  #
  #       this_issue <- component_issues[k, ]
  #       register_issue_dataproduct(handle, this_issue)
  #
  #       usethis::ui_done(paste("Writing", usethis::ui_value(components[j]),
  #                              usethis::ui_field("issue"),
  #                              "to local registry"))
  #     }
  #   }
  #
  #   # Attach issues to data product
  #
  #   dataproduct_issues <- handle$issues %>%
  #     dplyr::filter(is.na(component))
  #
  #   if (nrow(dataproduct_issues) != 0) {
  #     for (k in seq_len(nrow(dataproduct_issues))) {
  #
  #       this_issue <- dataproduct_issues[k, ]
  #       register_issue_dataproduct(handle, this_issue)
  #
  #       usethis::ui_done(paste("Writing", usethis::ui_value(components[j]),
  #                              usethis::ui_field("issue"),
  #                              "to local registry"))
  #     }
  #   }
  #
  # }

  # record the code run in the data registry --------------------------------
  patch_data(url = handle$code_run,
             data = list(inputs = as.list(handle$inputs$component_url),
                         outputs = as.list(handle$outputs$component_url)))

}
