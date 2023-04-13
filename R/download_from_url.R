#' Download source file from URL
#'
#' This function will download a file from a url
#' 
#' @keywords internal
#'
#' @param source_root a \code{string} specifying the source root
#' @param source_path a \code{string} specifying the source path
#' @param path a \code{string} specifying where the file will be saved
#' @param filename a \code{string} specifying the filename (the name given to
#' the saved file)
#'
#' @family download functions
#'
download_from_url <- function(source_root,
                              source_path,
                              path,
                              filename) {
  # Generate directory structure
  if (missing(path)) path <- getwd()
  if (!file.exists(path)) dir.create(path, recursive = TRUE)

  # Checks
  if (!grepl("/$", source_root))
    stop("The source_root argument should have a trailing slash")
  if (grepl("^/", source_path))
    stop("The source_path argument should not have a leading slash")

  # Download file
  download.file(url = paste0(source_root, source_path),
                destfile = file.path(path, filename), mode = "wb")
}
