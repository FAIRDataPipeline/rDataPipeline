#' register_external_object
#'
#' @param register_this metadata
#' @param datastore default local data storage location
#' @param namespace namespace
#' @param filename filename
#'
register_external_object <- function(register_this,
                                     datastore,
                                     namespace,
                                     filename) {

  hash <- strsplit(filename, "\\.")[[1]][1]

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

  usethis::ui_done(
    paste("Writing", usethis::ui_value(register_this$external_object),
          "download", usethis::ui_field("source"),"to local registry"))

  # Local store -------------------------------------------------------------

  # Add local data store root to the data registry
  datastore_root_id <- new_storage_root(
    name = "localstore",
    root = datastore,
    accessibility = 0) # TODO

  file_path <- file.path(namespace, register_this$product_name, filename)

  # Add local data store location to the data registry
  datastore_location_id <- new_storage_location(
    path = file_path,
    hash = hash,
    storage_root_id = datastore_root_id)

  datastore_object_id <- new_object(
    storage_location_id = datastore_location_id)

  # Source data (e.g. *.csv) is defined as having a single component
  # called "raw_data"
  datastore_component_id <- new_object_component(
    name = "raw_data",
    object_id = datastore_object_id)

  # Register external object ------------------------------------------------

  # Get release_date
  release_date <- register_this$release_date
  if ("DATETIME" %in% names(release_date)) {
    release_date <- Sys.time()
  }

  # Get version
  version <- register_this$version
  if (grepl("\\{DATETIME\\}", version)) {
    datetime <- gsub("-", "", Sys.Date())
    version <- gsub("\\{DATETIME\\}", datetime, version)
  }

  externalobject_id <- new_external_object(
    doi_or_unique_name = register_this$unique_name,
    primary_not_supplement = register_this$primary,
    release_date = release_date,
    title = register_this$title,
    description = register_this$description,
    version = version,
    object_id = datastore_object_id,
    source_id = source_id,
    original_store_id = source_location_id)

  stop_server()

  usethis::ui_done(
    paste("Writing", usethis::ui_value(register_this$external_object),
          "as", usethis::ui_field("external_object"), "to local registry"))

  invisible(datastore_component_id)
}
