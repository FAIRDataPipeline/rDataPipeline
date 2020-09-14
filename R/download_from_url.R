#' download_from_url
#'
#' This function will download a file from a url
#'
#' @param source_root a \code{string} specifying the source root *e.g.*
#' "https://www.nrscotland.gov.uk"
#' @param source_path a \code{string} specifying the source path *e.g.*
#' "files/statistics/population-estimates/sape-time-series/males/sape-2018-males.xlsx"
#' @param path a \code{string} specifying the local path (where to save the
#' file) *e.g.* "data-raw"
#' @param filename a \code{string} specifying the filename (the name given to
#' the saved file) *e.g.* "0.1.0.csv"
#' @param unzip a \code{boolean} specifying which when \code{TRUE} will
#' unzip a \code{.zip} file and remove the original \code{.zip} file
#'
#' @export
#'
#' @examples
#' \dontrun{
#' \donttest{
#' download_from_url(source_root = "https://www.nrscotland.gov.uk/",
#' source_path = file.path("files//statistics", "population-estimates",
#' "sape-time-series", "males", "sape-2018-males.xlsx"),
#' path = "data-raw",
#' filename = "0.1.0.csv")
#' }}
#'
download_from_url <- function(source_root,
                              source_path,
                              path,
                              filename,
                              unzip = FALSE) {
  # Generate directory structure
  if(!file.exists(path)) dir.create(path, recursive = TRUE)
  if(missing(path)) path <- ""

  # Checks
  if(!grepl("/$", source_root))
    stop("The source_root argument should have a trailing slash")
  if(grepl("^/", source_path))
    stop("The source_path argument should not have a prepending slash")

  # Download file
  download.file(url = paste(source_root, source_path, sep = "/"),
                destfile = file.path(path, filename), mode = "wb")

  # If file is zipped, unzip it and remove *.zip file
  if(grepl(".zip$", filename) & unzip) {
    unzip(file.path(path, filename), exdir = path)
    file.remove(file.path(path, filename))
  }

}
