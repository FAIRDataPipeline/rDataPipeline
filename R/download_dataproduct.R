#' download_dataproduct
#'
#' @param name a \code{string} specifying the name of the data product
#' @param data_dir a \code{string} specifying the download directory
#' @param version (optional) a \code{string} specifying the version number of
#' the data product; if version is not specified, the most recent version will
#' be downloaded
#'
#' @export
#'
#' @return Returns list comprising two elements
#' \itemize{
#'  \item{"downloaded_to"}{Where the file was downloaded to}
#'  \item{"components"}{Components contained within the file}
#' }
#' @examples
#' \dontrun{
#' \donttest{
#' # Automatically download the latest version
#' download_dataproduct("records/SARS-CoV-2/scotland/cases-and-management/testing",
#'  "data-raw")
#'
#' # Download specified version
#' download_dataproduct("records/SARS-CoV-2/scotland/cases-and-management/testing",
#'  "data-raw", "0.20200920.0")
#'
#' # Download only version
#' download_dataproduct("geography/scotland/lookup_table", "data-raw")
#' }}
#'
download_dataproduct <- function(name, data_dir, version) {
  entries <- get_entry("data_product", list(name = name))

  # Get latest version
  if(missing(version)) {
    version_numbers <- lapply(entries, function(x) x$version) %>% unlist()
    version <- max(version_numbers)
  }

  ind <- which(version_numbers == version)
  this_entry <- entries[[ind]]

  # Get object id
  object_id <- this_entry$object
  object_id <- gsub("https://data.scrc.uk/api/object/", "", object_id)
  object_id <- gsub("/", "", object_id)

  # Download file
  get_h5_from_object_id(as.numeric(object_id), data_dir)
}
