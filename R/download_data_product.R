#' Download data product
#'
#' Download data product from the Boydorr server.
#'
#' @param data_product a \code{string} specifying the name of the data product
#' @param version (optional) a \code{string} specifying the version number of
#' the data product; if version is not specified, the most recent version will
#' be downloaded
#' @param namespace namespace
#' @param data_dir a \code{string} specifying the download directory
#'
#' @family download functions
#'
#' @return Returns list comprising two elements
#' \itemize{
#'  \item{"downloaded_to"}{absolute path of H5 file after downloading}
#'  \item{"components"}{H5 file components}
#' }
#'
download_data_product <- function(data_product, version, namespace, data_dir) {

  run_server()

  # Where is the file -------------------------------------------------------

  namespace_id <- get_entry("namespace", list(name = namespace))[[1]]$url %>%
    clean_query() %>%
    unlist()

  entry <- get_entry("data_product", list(name = data_product,
                                          version = version,
                                          namespace = namespace_id))

  assertthat::assert_that(length(entry) == 1)
  assertthat::assert_that(!is.null(entry))

  object_url <- entry[[1]]$object
  object <- get_entity(object_url)
  storage_location <- get_storage_location(object$storage_location)

  stop_server()

  # Where should we save it -------------------------------------------------

  # If data_dir doesn't exist, create it
  if(!dir.exists(data_dir)) dir.create(data_dir, recursive = TRUE)

  dest_path <- file.path(data_dir, basename(storage_location$path))

  # If file already exists at this location, return a message. Otherwise,
  # download the file and return it's location and a list of components
  if(file.exists(dest_path)) {
    message("File already exists at this location")
  } else {
    download.file(url, dest_path, mode = "wb")
  }

  # Return ------------------------------------------------------------------

  list(downloaded_to = normalizePath(dest_path),
       components = get_components(storage_location))
}
