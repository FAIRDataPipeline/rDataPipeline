#' finalise
#'
#' @param handle \code{fdp} object
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

      if (file.exists(path)) {
        hash <- get_file_hash(path)
        tmp_filename <- basename(path)
        extension <- strsplit(tmp_filename, split = "\\.")[[1]][2]
        new_filename <- paste(hash, extension, sep = ".")
        new_path <- gsub(tmp_filename, new_filename, path)
        file.rename(path, new_path)

      } else {
        hash <- unique(this_metadata$hash)
        if (is.na(hash))
          usethis::ui_stop("Something is wrong")
      }

      # Read description from config.yaml

      index_dp <- which(unlist(lapply(handle$yaml$write, function(x)
        dp == x$data_product)))
      description <- handle$yaml$write[[index_dp]]$description

      namespace <- handle$yaml$run_metadata$default_output_namespace
      namespace_id <- new_namespace(name = namespace)

      # Read version from handle

      this_version <- unique(this_metadata$version)

      # Record file location in data registry

      storage_location <- gsub(datastore, "", path)

      dp_exists <- get_entry("data_product", list(name = dp,
                                                  version = this_version))

      if (is.null(dp_exists)) {
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

        usethis::ui_done(paste("Writing", usethis::ui_value(dp),
                               "to local registry"))

      } else {
        assertthat::assert_that(length(dp_exists) == 1)
        product_dataProductId <- dp_exists[[1]]$url
      }

      # Update handle
      handle$write_dataproduct_id(dp, product_dataProductId, this_version, hash)

      # Record components in data registry --------------------------------------

      components <- this_metadata$component

      for (j in seq_along(components)) {

        index_ct <- which(components[j] %in% this_metadata$component)
        this_component <- this_metadata$component[[index_ct]]

        component_id <- new_object_component(name = components[j],
                                             object_id = object_id)

        # Update handle
        handle$write_component_id(data_product = dp,
                                  component = components[j],
                                  component_id = component_id)

        usethis::ui_done(paste("Writing", usethis::ui_value(components[j]),
                               usethis::ui_field("component"),
                               "to local registry"))

      }

    }
  }


  issues <- handle$issues

  if (!is.null(issues)) {

    # Attach issues to components ---------------------------------------------

    component_issues <- handle$issues %>%
      dplyr::filter(!is.na(component))

    if (nrow(component_issues) != 0) {
      for (k in seq_len(nrow(component_issues))) {

        this_issue <- component_issues[k, ]
        register_issue_dataproduct(handle, this_issue)

        usethis::ui_done(paste("Writing", usethis::ui_value(components[j]),
                               usethis::ui_field("issue"),
                               "to local registry"))
      }
    }

    # Attach issues to data product

    dataproduct_issues <- handle$issues %>%
      dplyr::filter(is.na(component))

    if (nrow(dataproduct_issues) != 0) {
      for (k in seq_len(nrow(dataproduct_issues))) {

        this_issue <- dataproduct_issues[k, ]
        register_issue_dataproduct(handle, this_issue)

        usethis::ui_done(paste("Writing", usethis::ui_value(components[j]),
                               usethis::ui_field("issue"),
                               "to local registry"))
      }
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

  patch_data(url = handle$code_run,
             data = list(inputs = as.list(handle$inputs$object_id),
                         outputs = as.list(handle$outputs$component_id)))

  # coderun_id <- new_coderun(run_date = Sys.time(),
  #                           description = handle$yaml$run_metadata$description,
  #                           # code_repo_id = "",
  #                           model_config = handle$model_config,
  #                           submission_script_id = handle$submission_script,
  #                           inputs = as.list(handle$inputs$object_id),
  #                           outputs = as.list(handle$outputs$component_id))



  stop_server()
}
