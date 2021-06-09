#' register_external_object
#'
#' @param register_this metadata
#' @param datastore a \code{string} specifying the default local data storage
#' location
#' @param namespace a \code{string} specifying the name of the namespace
#' @param filename a \code{string} specifying the filename
#'
register_external_object <- function(register_this,
                                     datastore,
                                     namespace,
                                     filename) {

  hash <- strsplit(filename, "\\.")[[1]][1]

  # Original source ---------------------------------------------------------

  run_server()

  # Add the website to the data registry (e.g. home page of the database)
  source_uri <- new_source(
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

  source_root_uri <- new_storage_root(
    name = register_this$root_name,
    root = register_this$root,
    accessibility = accessibility)

  # Add source location to the data registry
  source_location_uri <- new_storage_location(
    path = register_this$path,
    hash = hash,
    storage_root_uri = source_root_uri)

  usethis::ui_done(
    paste("Writing", usethis::ui_value(register_this$external_object),
          "download", usethis::ui_field("source"),"to local registry"))

  # Local store -------------------------------------------------------------

  # Does this file already exist?
  file_path <- file.path(namespace, register_this$product_name, filename)

  # Add local data store root to the data registry
  datastore_name <- paste("local datastore:", datastore)
  datastore_root_uri <- new_storage_root(name = datastore_name,
                                         root = datastore,
                                         accessibility = 0) # TODO

  # Add local data store location to the data registry
  datastore_location_uri <- new_storage_location(
    path = file_path,
    hash = hash,
    storage_root_uri = datastore_root_uri)

  datastore_object_uri <- new_object(
    storage_location_uri = datastore_location_uri)

  # Source data (e.g. *.csv) is defined as having a single component
  # called "raw_data"
  datastore_component_uri <- new_object_component(
    name = "raw_data",
    object_uri = datastore_object_uri)

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

  externalobject_uri <- new_external_object(
    doi_or_unique_name = register_this$unique_name,
    primary_not_supplement = register_this$primary,
    release_date = release_date,
    title = register_this$title,
    description = register_this$description,
    version = version,
    object_uri = datastore_object_uri,
    source_uri = source_uri,
    original_store_uri = source_location_uri)

  stop_server()

  usethis::ui_done(
    paste("Writing", usethis::ui_value(register_this$external_object),
          "as", usethis::ui_field("external_object"), "to local registry"))

  invisible(datastore_component_uri)
}
