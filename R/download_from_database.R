#' Download source file from database
#' 
#' @keywords internal
#'
#' @param source_root a \code{string} specifying the source root
#' @param source_path a \code{string} specifying the source path
#' @param path a \code{string} specifying where the file will be saved
#' @param filename a \code{string} specifying the filename (the name given to
#' the saved file)
#' @param overwrite a \code{boolean} specifying whether or not the file should
#' be overwritten if it already exists
#'
#' @family download functions
#'
download_from_database <- function(source_root,
                                   source_path,
                                   path,
                                   filename,
                                   overwrite = FALSE) {
    # Generate directory structure
    if (!file.exists(path)) dir.create(path, recursive = TRUE)
    if (missing(path)) path <- ""

    # Download file
    httr::GET(paste0(source_root,
                     utils::URLencode(source_path, reserved = TRUE)),
              httr::content_type("text/csv"),
              httr::write_disk(file.path(path, filename),
                               overwrite = overwrite))
}
