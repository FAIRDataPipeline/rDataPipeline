#' store_script
#'
#' @param script string
#' @param path filename
#'
#' @export
#'
store_script <- function(script, path) {
  # If scripts directory doesn't exist, create it
  directory <- dirname(path)
  if (!file.exists(directory))
    dir.create(directory)

  # Write script file (increment file name if it exists - why would it though?)
  path <- increment_filename(path)

  cat(script, file = path)
  invisible(path)
}
