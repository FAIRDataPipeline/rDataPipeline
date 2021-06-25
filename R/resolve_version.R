#' resolve_version
#' @param version version number
#'
resolve_version <- function(version) {
  # Take care of variables
  if (grepl("\\$\\{\\{PATCH\\}\\}", version)) {
    stop("Not written")

  } else if (grepl("\\$\\{\\{MINOR\\}\\}", version)) {
    stop("Not written")

  } else if (grepl("\\$\\{\\{MAJOR\\}\\}", version)) {
    stop("Not written")

  } else if (grepl("\\$\\{\\{DATETIME\\}\\}", version)) {
    datetime <- gsub("-", "", Sys.Date())
    write_version <- gsub("\\$\\{\\{DATETIME\\}\\}", datetime,
                          version)

  } else {
    write_version <- version
  }

  write_version
}
