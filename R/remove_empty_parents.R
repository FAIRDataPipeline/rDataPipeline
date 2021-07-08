#' remove_empty_parents
#' @param path path
#' @param root root
#'
remove_empty_parents <- function(path, root) {
  root <- gsub("/", "", root)
  directory <- dirname(path)
  continue <- directory != root

  while (continue) {
    empty <- length(dir(directory)) == 0
    if (empty) {
      unlink(directory, recursive = TRUE)
      directory <- dirname(directory)
      continue <- directory != root
    } else {
      continue <- FALSE
    }
  }
  invisible(TRUE)
}
