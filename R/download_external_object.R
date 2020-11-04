#' Download external object
#'
#' Download external object from the Boydorr server.
#'
#' @param name a \code{string} specifying the name of the external object
#' @param data_dir a \code{string} specifying the download directory
#' @param version (optional) a \code{string} specifying the version number of
#' the external object; if version is not specified, the most recent version
#' will be downloaded
#' @param unzip a \code{boolean} specifying which when \code{TRUE} will
#' unzip a \code{.zip} file and remove the original \code{.zip} file
#'
#' @return Returns list comprising two elements
#' \itemize{
#'  \item{"downloaded_to"}{absolute path of H5 file after downloading}
#'  \item{"components"}{H5 file components}
#' }
#'
#' @family download functions
#'
#' @export
#'
#' @examples
#' \dontrun{
#' external_object <- "scottish coronavirus-covid-19-management-information"
#'
#' # Automatically download the latest version
#' download_external_object(name = external_object,
#'                          data_dir = "data-raw")
#'
#' # Download specified version
#' download_external_object(name = external_object,
#'                          data_dir = "data-raw",
#'                          version = "0.20200920.0")
#' }
#'
download_external_object <- function(name,
                                     data_dir,
                                     version,
                                     unzip = FALSE) {
  # List all version numbers in the data registry
  entries <- get_entry("external_object", list(doi_or_unique_name = name))

  if(!is.null(entries)) {
    version_numbers <- lapply(entries, function(x) x$version) %>%
      unlist()

    # If version hasn't been input, get the latest version from the data registry
    if(missing(version)) version <- max(version_numbers)

    # Find the version
    ind <- which(version_numbers == version)
    this_entry <- entries[[ind]]

    # Get its object id
    object_id <- this_entry$object
    object_id <- gsub("https://data.scrc.uk/api/object/", "", object_id)
    object_id <- gsub("/", "", object_id)

    # Download file
    out <- get_data_from_object_id(as.numeric(object_id), data_dir)

    # If file is zipped, unzip it and remove *.zip file
    if(grepl(".zip$", out$downloaded_to) & unzip) {
      exdir <-  gsub(".zip$", "", out$downloaded_to)
      unzip(out$downloaded_to, exdir = exdir)
      file.remove(out$downloaded_to)
    }

    return(out)
  }
}
