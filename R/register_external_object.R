#' register_external_object
#'
#' @param register_this metadata
#' @param datastore default local data storage location
#' @param namespace
#'
register_external_object <- function(register_this,
                                     datastore,
                                     namespace) {

  # Download data -----------------------------------------------------------

  # Local data store location
  local_path <- file.path(paste0(datastore, namespace), register_this$product_name)
  tmp_filename <- paste(openssl::sha1(as.character(Sys.time())),
                        register_this$file_type, sep = ".")

  if (grepl("SELECT", register_this$path) &
      grepl("WHERE", register_this$path)) {
    download_from_database(source_root = register_this$root,
                           source_path = register_this$path,
                           path = local_path,
                           filename = tmp_filename)
  }

  # Rename data file
  hash <- get_file_hash(file.path(local_path, tmp_filename))
  new_filename <- paste(hash, register_this$file_type, sep = ".")
  file.rename(file.path(local_path, tmp_filename),
              file.path(local_path, new_filename))

  # Original source ---------------------------------------------------------

  run_server()

  # Add the website to the data registry (e.g. home page of the database)
  source_id <- new_source(
    name = register_this$source_name,
    abbreviation = register_this$source_abbreviation,
    website = register_this$source_website)

  # Add source root to the data registry (e.g. an endpoint)
  if (register_this$accessibility == "open") {
    accessibility <- 0
  } else if (register_this$accessibility == "closed") {
    accessibility <- 1
  } else
    usethis::ui_oops("Unknown accessibility value")

  source_root_id <- new_storage_root(
    name = register_this$root_name,
    root = register_this$root,
    accessibility = accessibility)

  # Add source location to the data registry
  source_location_id <- new_storage_location(
    path = register_this$path,
    hash = hash,
    storage_root_id = source_root_id)

  # Local store -------------------------------------------------------------

  # Add local data store root to the data registry
  datastore_root_id <- new_storage_root(
    name = "localstore",
    root = datastore,
    accessibility = 0) # TODO

  # Add local data store location to the data registry
  datastore_location_id <- new_storage_location(
    path = file.path(namespace, register_this$product_name,
                     new_filename),
    hash = hash,
    storage_root_id = datastore_root_id)

  datastore_object_id <- new_object(storage_location_id = datastore_location_id)

  # Source data (e.g. *.csv) is defined as having a single component
  # called "raw_data"
  datastore_component_id <- new_object_component(name = "raw_data",
                                                 object_id = datastore_object_id)

  # Register external object ------------------------------------------------

  externalobject_id <- new_external_object(
    doi_or_unique_name = register_this$unique_name,
    primary_not_supplement = register_this$primary,
    release_date = register_this$release_date,
    title = register_this$title,
    description = register_this$description,
    version = register_this$version,
    object_id = datastore_object_id,
    source_id = source_id,
    original_store_id = source_location_id)

  stop_server()

  usethis::ui_done(
    paste("Add", usethis::ui_value(register_this$external_object),
          "as", usethis::ui_field("external_object"), "in local registry"))

  invisible(datastore_component_id)
}
