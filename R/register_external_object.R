#' register_external_object
#'
#' @param yaml config yaml
#' @param register_this metadata
#' @param endpoint endpoint
#'
register_external_object <- function(yaml,
                                     register_this,
                                     endpoint) {

  datastore <- yaml$run_metadata$write_data_store
  namespace <- yaml$run_metadata$default_output_namespace
  public <- register_this$public

  # Local data store location
  local_path <- file.path(paste0(datastore, namespace),
                          register_this$external_object)
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
    download_from_url(source_root = register_this$root,
                      source_path = register_this$path,
                      path = local_path,
                      filename = tmp_filename)
  }

  usethis::ui_done(
    paste("Downloading", usethis::ui_value(register_this$external_object),
          "to local data store"))

  # Rename data file and record location of data product in registry --------

  hash <- get_file_hash(file.path(local_path, tmp_filename))

  # Does this file already exist?
  new_filename <- paste(hash, register_this$file_type, sep = ".")
  file_exists <- file.exists(new_filename)

  # Rename file or delete it
  if (!file_exists) {
    file.rename(file.path(local_path, tmp_filename),
                file.path(local_path, new_filename))
  } else {
    file.remove(file.path(local_path, tmp_filename))
  }

  # Add local data store root to the data registry
  datastore_root_url <- new_storage_root(root = datastore,
                                         local = TRUE,
                                         endpoint = endpoint)

  file_path <- file.path(namespace, register_this$external_object,
                         new_filename)

  # Add local data store location to the data registry
  datastore_location_url <- new_storage_location(
    path = file_path,
    hash = hash,
    public = TRUE,
    storage_root_url = datastore_root_url,
    endpoint = endpoint)

  # Get external object metadata --------------------------------------------

  # Get release_date
  release_date <- register_this$release_date
  if ("${{CLI.DATETIME}}" == release_date) {
    release_date <- Sys.time()
  }

  # Get version
  register_version <- register_this$version
  if (grepl("\\$\\{\\{CLI.DATE\\}\\}", register_version)) {
    datetime <- gsub("-", "", Sys.Date())
    register_version <- gsub("\\$\\{\\{CLI.DATE\\}\\}", datetime,
                             register_version)
  }

  # Get namespace
  register_namespace_url <- new_namespace(name = namespace,
                                          full_name = namespace,
                                          endpoint = endpoint)
  register_namespace_id <- extract_id(register_namespace_url)

  # Get data_product
  register_data_product <- register_this$external_object

  # Does the data product already exist?
  data_product_exists <- get_entry("data_product",
                                   list(name = register_data_product,
                                        version = register_version,
                                        namespace = register_namespace_id),
                                   endpoint = endpoint)

  if (is.null(data_product_exists)) {
    # Original source ---------------------------------------------------------

    # Add the website to the data registry (e.g. home page of the database)
    source_url <- new_namespace(
      name = register_this$source_abbreviation,
      full_name = register_this$source_name,
      website = register_this$source_website,
      endpoint = endpoint)

    source_root_url <- new_storage_root(
      root = register_this$root,
      local = FALSE,
      endpoint = endpoint)

    # Add source location to the data registry
    source_location_url <- new_storage_location(
      path = register_this$path,
      hash = hash,
      public = TRUE,
      storage_root_url = source_root_url,
      endpoint = endpoint)

    usethis::ui_done(
      paste("Writing", usethis::ui_value(register_this$external_object),
            "download source to local registry"))

    # Register external object ------------------------------------------------

    filetype_exists <- get_url("file_type",
                               list(extension = register_this$file_type),
                               endpoint = endpoint)

    if (is.null(filetype_exists)) {
      filetype_url <- new_file_type(name = register_this$file_type,
                                    extension = register_this$file_type,
                                    endpoint = endpoint)
    } else {
      assertthat::assert_that(length(filetype_exists) == 1)
      filetype_url <- filetype_exists
    }

    datastore_object_url <- new_object(
      description = register_this$description,
      storage_location_url = datastore_location_url,
      file_type_url = filetype_url,
      endpoint = endpoint)

    # Get user metadata
    user_url <- get_url(table = "users",
                        query = list(username = "admin"),
                        endpoint = endpoint)
    assertthat::assert_that(length(user_url) == 1)
    user_id <- extract_id(user_url)
    user_author_org_url <- get_entry("user_author_org",
                                     query = list(user = user_id),
                                     endpoint = endpoint)
    assertthat::assert_that(length(user_author_org_url) == 1)
    author_url <- user_author_org_url[[1]]$author
    organisations_urls <- user_author_org_url[[1]]$organisations

    new_object_author_org(
      object_url = datastore_object_url,
      author_url = author_url,
      organisations_urls = organisations_urls,
      endpoint = endpoint)

    data_product_url <- new_data_product(name = register_data_product,
                                         version = register_version,
                                         object_url = datastore_object_url,
                                         namespace_url = register_namespace_url,
                                         endpoint = endpoint)

    externalobject_url <- new_external_object(
      doi_or_unique_name = register_this$unique_name,
      primary_not_supplement = register_this$primary,
      release_date = release_date,
      title = register_this$title,
      description = register_this$description,
      data_product_url = data_product_url,
      original_store_url = source_location_url,
      endpoint = endpoint)

    usethis::ui_done(
      paste("Writing", usethis::ui_value(register_this$external_object),
            "as", usethis::ui_field("external_object"), "to local registry"))
  }

}
