#' increment_filename
#'
#' Searches scriptdir for duplicate files and increments filename.
#'
#' @param scriptdir directory
#' @param filename file name
#'
#' @export
#'
increment_filename <- function(scriptdir, filename) {

  if (file.exists(file.path(scriptdir, filename))) {
    all_files <- dir(scriptdir)
    no_extension <- strsplit(filename, "\\.")[[1]][1]
    extension <- strsplit(filename, "\\.")[[1]][2]
    duplicates <- all_files[grepl(no_extension, all_files)]

    have_brackets <- grepl("\\([0-9]+\\).[a-zA-Z0-9]+$", duplicates)
    if (any(have_brackets)) {
      number <- gsub("[0-9]+-[0-9]+\\(([0-9]+)\\).[a-zA-Z0-9]+$", "\\1",
                     duplicates[have_brackets])
      number <- max(as.numeric(number)) + 1
      output <- paste0(no_extension, "(", number, ").", extension)

    } else {
      output <- paste0(no_extension, "(2).", extension)
    }
  } else {
    output <- filename
  }
  output
}
