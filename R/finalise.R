#' finalise
#'
#' @param handle list
#'
finalise <- function(handle) {

  # record data product metadata (e.g. location, components, various descriptions, issues) in the data registry --------

  run_server()

  # Add local data store root to the data registry

  datastore <- handle$yaml$run_metadata$default_data_store
  datastore_root_id <- new_storage_root(
    name = "localstore",
    root = datastore,
    accessibility = 0) # TODO

  # rename the data product as {hash}.h5 -----------------------------------

  if (!is.null(handle$outputs)) {

    data_products <- unique(handle$outputs$data_product)

    for (i in seq_along(data_products)) {

      dp <- data_products[i]
      this_metadata <- handle$outputs %>%
        dplyr::filter(.data$data_product == dp)

      # Get file path

      path <- this_metadata %>%
        dplyr::select(.data$path) %>%
        unique() %>% unlist() %>% unname()

      # Rename file

      hash <- get_file_hash(path)
      tmp_filename <- basename(path)
      extension <- strsplit(tmp_filename, split = "\\.")[[1]][2]
      new_filename <- paste(hash, extension, sep = ".")
      new_path <- gsub(tmp_filename, new_filename, path)
      file.rename(path, new_path)

      # Read version and description from config.yaml

      index_dp <- which(unlist(lapply(handle$yaml$write, function(x)
        dp == x$data_product)))
      if (length(index_dp) == 0)
        stop("Data product not present in config.yaml")

      this_version <- handle$yaml$write[[index_dp]]$version
      description <- handle$yaml$write[[index_dp]]$description

      namespace <- handle$yaml$run_metadata$default_output_namespace
      namespace_id <- new_namespace(name = namespace)

      # Record file location in data registry

      storage_location <- gsub(datastore, "", path)

      storage_location_id <- new_storage_location(
        path = storage_location,
        hash = hash,
        storage_root_id = datastore_root_id)

      object_id <- new_object(storage_location_id = storage_location_id,
                              description = description)

      # Add data product to data registry

      product_dataProductId <- new_data_product(name = dp,
                                                version = this_version,
                                                object_id = object_id,
                                                namespace_id = namespace_id)

      # Update handle
      handle$write_dataproduct_id(dp, product_dataProductId)

      # Record components in data registry

      components <- this_metadata$component

      for (j in seq_along(components)) {

        index_ct <- which(components[j] %in% this_metadata$component)
        this_component <- this_metadata$components[[index_ct]]

        component_id <- new_object_component(name = components[j],
                                             object_id = object_id)

        # Update handle
        handle$write_component_id(data_product = dp,
                                  component = components[j],
                                  component_id = component_id)

        # Attach issues to component

        if (!is.na(this_metadata$issue[j])) {
          issue <- this_metadata$issue[j]
          severity <- this_metadata$severity[j]

          register_issue_component(issue = issue,
                                   severity = severity,
                                   data_product = dp,
                                   namespace = namespace,
                                   component = components[j],
                                   version = this_version)
        }
      }

      # Attach issues to data product

      # if (any("issues" %in% names(writes[[index_dp]]))) {
      #   issue <- writes[[index_dp]]$issues
      #   severity <- writes[[index_dp]]$severity
      #
      #   issue_with_dataproduct(issue,
      #                          severity,
      #                          dp,
      #                          namespace,
      #                          this_version)
      # }
    }
  }


  # link objects together

  # githubRepoURIs <- upload_github_repo(
  #  storage_root_id = repo_storageRootId,
  #  repo = github_info$script_gitRepo,
  #  hash = github_info$github_hash,
  #  version = github_info$repo_version,
  #  key = key)

  # record the code run in the data registry --------------------------------

  coderun_id <- new_coderun(run_date = Sys.time(),
                            description = handle$yaml$run_metadata$description,
                            # code_repo_id = "",
                            model_config = handle$model_config,
                            submission_script_id = handle$submission_script,
                            inputs = as.list(handle$inputs$object_id),
                            outputs = as.list(handle$outputs$component_id))

  usethis::ui_done(paste("Record", usethis::ui_value("code run"),
                         "in local registry"))

  stop_server()
}
