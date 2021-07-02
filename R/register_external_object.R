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

  # Add the website to the data registry (e.g. home page of the database)
  source_url <- new_namespace(
    name = register_this$source_abbreviation,
    full_name = register_this$source_name,
    website = register_this$source_website)

  source_root_url <- new_storage_root(
    root = register_this$root,
    local = FALSE)

  # Add source location to the data registry
  source_location_url <- new_storage_location(
    path = register_this$path,
    hash = hash,
    storage_root_url = source_root_url)

  usethis::ui_done(
    paste("Writing", usethis::ui_value(register_this$external_object),
          "download source to local registry"))

  # Local store -------------------------------------------------------------

  # Add local data store root to the data registry
  datastore_root_url <- new_storage_root(root = datastore,
                                         local = TRUE)

  # Does this file already exist?
  file_path <- file.path(namespace, register_this$external_object, filename)

  # Add local data store location to the data registry
  datastore_location_url <- new_storage_location(
    path = file_path,
    hash = hash,
    storage_root_url = datastore_root_url)

  filetype_exists <- get_url("file_type",
                             list(extension = register_this$file_type))

  if (is.null(filetype_exists)) {
    filetype_url <- new_file_type(name = register_this$file_type,
                                  extension = register_this$file_type)
  } else {
    assertthat::assert_that(length(filetype_exists) == 1)
    filetype_url <- filetype_exists
  }

  datastore_object_url <- new_object(
    description = register_this$description,
    storage_location_url = datastore_location_url,
    file_type_url = filetype_url)

  # Register external object ------------------------------------------------

  # Get release_date
  release_date <- register_this$release_date
  if ("${{CLI.DATE}}" == release_date) {
    release_date <- Sys.time()
  }

  # Get version
  version <- register_this$version
  if (grepl("\\$\\{\\{CLI.DATE\\}\\}", version)) {
    datetime <- gsub("-", "", Sys.Date())
    version <- gsub("\\$\\{\\{CLI.DATE\\}\\}", datetime, version)
  }

  namespace_url <- new_namespace(name = namespace,
                                 full_name = namespace)

  data_product_url <- new_data_product(name = register_this$external_object,
                                       version = version,
                                       object_url = datastore_object_url,
                                       namespace_url = namespace_url)

  externalobject_url <- new_external_object(
    doi_or_unique_name = register_this$unique_name,
    primary_not_supplement = register_this$primary,
    release_date = release_date,
    title = register_this$title,
    description = register_this$description,
    data_product_url = data_product_url,
    original_store_url = source_location_url)

  usethis::ui_done(
    paste("Writing", usethis::ui_value(register_this$external_object),
          "as", usethis::ui_field("external_object"), "to local registry"))

  invisible(TRUE)
}
