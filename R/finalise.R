#' finalise
#'
#' @param handle list
#'
finalise <- function(handle) {

  run_server()

  # Add local data store root to the data registry --------------------------

  datastore <- handle$yaml$run_metadata$default_data_store
  datastore_root_id <- new_storage_root(
    name = "localstore",
    root = datastore,
    accessibility = 0) # TODO

  # -------------------------------------------------------------------------

  if (nrow(handle$outputs) != 0) {

    data_products <- unique(handle$outputs$dataproduct)

    for (i in seq_along(data_products)) {

      data_product <- data_products[i]
      path <- handle$outputs %>%
        dplyr::filter(.data$dataproduct == data_product) %>%
        dplyr::select(.data$path) %>%
        unique() %>%
        unlist() %>%
        unname()
      storage_location <- gsub(datastore, "", path)

      writes <- handle$yaml$write
      index_dp <- which(unlist(lapply(writes, function(x)
        data_product == x$data_product)))
      if (length(index_dp) == 0)
        stop("Data product not present in config.yaml")

      # Compare components in data product to those in config.yaml file ------

      if(grepl(".h5$", path)) {
        components <- get_components(path)
      } else if(grepl(".toml$", path)) {
        # components <- data.frame(name = component_name)
        stop("Not written yet")
      } else {
        stop("Why is your data product not a toml or an h5 file?")
      }

      components_in_yaml <- names(writes[[index_dp]]$components)
      mismatched <- components_in_yaml[!which(components_in_yaml %in% components)]

      if (length(mismatched) != 0)
        usethis::ui_stop(paste("Mismatched components in config.yaml:",
                               usethis::ui_value(mismatched)))

      # Rename file ---------------------------------------------------------

      hash <- get_file_hash(path)
      tmp_filename <- basename(path)
      extension <- strsplit(tmp_filename, split = "\\.")[[1]][2]
      new_filename <- paste(hash, extension, sep = ".")
      new_path <- gsub(tmp_filename, new_filename, path)
      file.rename(path, new_path)

      # Read version and description from config.yaml ------------------------

      this_version <- writes[[index_dp]]$version
      description <- writes[[index_dp]]$description

      namespace <- handle$yaml$run_metadata$default_output_namespace
      namespace_id <- new_namespace(name = namespace)

      # Record file location to data registry --------------------------------

      storage_location_id <- new_storage_location(
        path = storage_location,
        hash = get_file_hash(new_path),
        storage_root_id = datastore_root_id)

      object_id <- new_object(storage_location_id = storage_location_id,
                              description = description)

      # Add data product to data registry ------------------------------------

      product_dataProductId <- new_data_product(name = data_product,
                                                version = this_version,
                                                object_id = object_id,
                                                namespace_id = namespace_id)
      handle$write_dataproduct_id(data_product, product_dataProductId)

      # Record components in data registry -----------------------------------

      for (j in seq_along(components)) {

        this_dataproduct <- writes[[index_dp]]
        index_ct <- which(components[j] == names(this_dataproduct$components))
        if (length(index_ct) == 0)
          stop("Component not present in config.yaml")
        this_component <- this_dataproduct$components[[index_ct]]

        component_id <- new_object_component(name = components[j],
                                             object_id = object_id)

        # Update handle
        handle$write_component_id(data_product, components[j], component_id)

        # Attach issues to component -----------------------------------------

        if (any("issues" %in% names(this_component))) {
          issue <- this_component$issues
          severity <- this_component$severity

          issue_with_component(issue = issue,
                               severity = severity,
                               data_product = data_product,
                               namespace = namespace,
                               component = components[j],
                               version = this_version)
        }
      }

      # Attach issues to data product ----------------------------------------

      if (any("issues" %in% names(writes[[index_dp]]))) {
        issue <- writes[[index_dp]]$issues
        severity <- writes[[index_dp]]$severity

        issue_with_dataproduct(issue,
                               severity,
                               data_product,
                               namespace,
                               this_version)
      }
    }
  }


  # link objects together ---------------------------------------------------

  # githubRepoURIs <- upload_github_repo(
  #  storage_root_id = repo_storageRootId,
  #  repo = github_info$script_gitRepo,
  #  hash = github_info$github_hash,
  #  version = github_info$repo_version,
  #  key = key)

  # Create new code_run -----------------------------------------------------

  coderun_id <- new_coderun(run_date = Sys.time(),
                            description = handle$yaml$run_metadata$description,
                            # code_repo_id = "",
                            model_config = handle$model_config,
                            submission_script_id = handle$submission_script,
                            inputs = unname(handle$inputs),
                            outputs = as.list(handle$outputs$component_id))

  usethis::ui_done(paste("Record", usethis::ui_value("code run"),
                         "in local registry"))

  stop_server()
}
