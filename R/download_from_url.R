#' Download source file from URL
#'
#' This function will download a file from a url
#'
#' @param source_root a \code{string} specifying the source root
#' @param source_path a \code{string} specifying the source path
#' @param path a \code{string} specifying where the file will be saved
#' @param filename a \code{string} specifying the filename (the name given to
#' the saved file)
#' @param unzip a \code{boolean} specifying which when \code{TRUE} will
#' unzip a \code{.zip} file and remove the original \code{.zip} file
#'
#' @family download functions
#'
#' @export
#'
#' @examples
#' \donttest{
#' \dontrun{
#' # Download the file
#' source_path <- "downloads/file?id=5a9bf61e-7571-45e8-a307-7c1218d5f6b5%2FDatazone2011Lookup.csv"
#' download_from_url(source_root = "http://statistics.gov.scot/",
#'                   source_path = source_path,
#'                   path = "geography/scotland/lookup_table",
#'                   filename = "0.1.0.csv")
#'
#' # Delete the file
#' file.remove("geography/scotland/lookup_table/0.1.0.csv")
#' }}
#'
download_from_url <- function(source_root,
                              source_path,
                              path,
                              filename,
                              unzip = FALSE) {
  # Generate directory structure
  if(missing(path)) path <- getwd()
  if(!file.exists(path)) dir.create(path, recursive = TRUE)


  # Checks
  if(!grepl("/$", source_root))
    stop("The source_root argument should have a trailing slash")
  if(grepl("^/", source_path))
    stop("The source_path argument should not have a leading slash")

  # Download file
  download.file(url = paste0(source_root, source_path),
                destfile = file.path(path, filename), mode = "wb")

  # If file is zipped, unzip it and remove *.zip file
  if(grepl(".zip$", filename) & unzip) {
    unzip(file.path(path, filename), exdir = path)
    file.remove(file.path(path, filename))
  }

}
