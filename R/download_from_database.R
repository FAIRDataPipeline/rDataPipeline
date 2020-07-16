#' download_from_database
#'
#' @param source_root source_root
#' @param source_path source_path
#' @param filename filename
#' @param path path
#' @param overwrite overwrite
#'
#' @export
#'
download_from_database <- function(source_root,
                                   source_path,
                                   filename,
                                   path,
                                   overwrite = FALSE) {
  # Generate directory structure
  if(!file.exists(path)) dir.create(path, recursive = TRUE)
  if(missing(path)) path <- ""

  # Download file
  httr::GET(paste0(source_root,
                   utils::URLencode(source_path, reserved = TRUE)),
            httr::content_type("text/csv"),
            httr::write_disk(file.path(path, filename), overwrite = overwrite))
}
