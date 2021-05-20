#' fdp_pull
#'
#' @export
#'
fdp_pull <- function(path = "config.yaml") {

  # Read config.yaml --------------------------------------------------------

  if (file.exists(path)) {
    yaml <- yaml::read_yaml(path)
    usethis::ui_done("Read config.yaml")
  } else
    usethis::ui_stop(paste(usethis::ui_value(path), "does not exist"))

  datastore <- yaml$run_metadata$default_data_store
  namespace <- yaml$run_metadata$default_output_namespace

  # Download any data required by `read:` from the remote data store and record
  # metadata in the data registry -------------------------------------------



  # Pull data associated with all previous versions of these objects from the
  # remote data registry ----------------------------------------------------



  # Download any data listed in register: from the original source and record
  # metadata in the data registry -------------------------------------------

  # Check for presence of `register` key

  if (any("register" %in% names(yaml))) {
    register <- yaml$register

    # If names(register) is null, then a single entry has been added that is
    # not in a list, so put it in a list
    if (!all(is.null(names(register))))
      register <- list(register)

    # Find entry to register
    types <- c("external_object", "data_product", "object")
    entries <- lapply(register, function(x)
      lapply(types, function(y) x[[y]]) %>% unlist())

    # Download raw data to data store and register in data registry
    for (i in seq_along(register)) {

      register_this <- register[[i]]

      # Local data store location
      local_path <- file.path(paste0(datastore, namespace),
                              register_this$product_name)
      tmp_filename <- paste(openssl::sha1(as.character(Sys.time())),
                            register_this$file_type, sep = ".")

      # Download data
      if (grepl("SELECT", register_this$path) &
          grepl("WHERE", register_this$path)) {
        download_from_database(source_root = register_this$root,
                               source_path = register_this$path,
                               path = local_path,
                               filename = tmp_filename)
      } else {
        stop("Code not written")
      }

      # Rename data file
      hash <- get_file_hash(file.path(local_path, tmp_filename))
      new_filename <- paste(hash, register_this$file_type, sep = ".")
      file.rename(file.path(local_path, tmp_filename),
                  file.path(local_path, new_filename))

      usethis::ui_done(
        paste("Download", usethis::ui_value(register_this$external_object),
              "to local data store"))

      # Update local data registry

      if ("external_object" %in% names(register_this)) {

        externalobject_id <- register_external_object(
          register_this = register_this,
          datastore = datastore,
          namespace = namespace,
          filename = new_filename)
      } else {
        stop("Code not written")
      }

    }
  }

}
