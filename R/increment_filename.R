#' increment_filename
#'
#' Searches directory for duplicate files and increments filename.
#'
#' @param path path
#'
increment_filename <- function(path) {

  directory <- dirname(path)
  filename <- basename(path)
  justname <- gsub("(.+)(\\([0-9]+\\))*\\.[a-zA-Z0-9]+", "\\1", filename)
  extension <- strsplit(filename, "\\.")[[1]][2]

  if (file.exists(path)) {
    all_files <- dir(directory)
    copies <- all_files[grepl(justname, all_files)]

    have_brackets <- grepl("\\([0-9]+\\).[a-zA-Z0-9]+$", copies)
    if (any(have_brackets)) {
      number <- gsub(".+\\(([0-9]+)\\)\\.[a-zA-Z0-9]+$", "\\1",
                     copies[have_brackets])
      number <- max(as.numeric(number)) + 1
      new_filename <- paste0(justname, "(", number, ").", extension)

    } else {
      new_filename <- paste0(justname, "(2).", extension)
    }
  } else {
    new_filename <- filename
  }
  file.path(directory, new_filename)
}
