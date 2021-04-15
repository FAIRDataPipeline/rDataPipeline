#' finalise
#'
#' @param handle list
#'
finalise <- function(handle) {

  run_server()

  # Add local data store root to the data registry
  datastore <- handle$yaml$run_metadata$default_data_store
  datastore_root_id <- new_storage_root(
    name = "localstore",
    root = datastore,
    accessibility = 0) # TODO

  # upload data product metadata to the registry ----------------------------

  outputs <- list()

  if (!is.null(handle$outputs$dataproducts)) {

    for (i in seq_along(handle$outputs$dataproducts)) {
      dataproduct <- names(handle$outputs$dataproducts)[i]
      path <- handle$outputs$dataproducts[[i]]$path
      storage_location <- gsub(datastore, "", path)

      # Read description from config.yaml
      writes <- handle$yaml$write
      index <- which(unlist(lapply(writes, function(x)
        dataproduct == x$data_product)))
      this_version <- writes[[index]]$version
      description <- writes[[index]]$description

      namespace <- handle$yaml$run_metadata$default_output_namespace
      namespace_id <- new_namespace(name = namespace)

      dataproduct_id <- upload_data_product(
        storage_root_id = datastore_root_id,
        name = dataproduct,
        description = description,
        processed_path = path,
        product_path = storage_location,
        version = this_version,
        namespace_id = namespace_id)
      outputs <- c(outputs, dataproduct_id)
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
                            outputs = outputs)

  usethis::ui_done(paste("Record", usethis::ui_value("code run"),
                         "in local registry"))

  stop_server()

}
