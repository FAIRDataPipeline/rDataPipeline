#' resolve_version
#' @param version version number
#' @param data_product
#' @param namespace_id
#'
resolve_version <- function(version, data_product, namespace_id) {

  # Increment patch ---------------------------------------------------------

  if (grepl("\\$\\{\\{PATCH\\}\\}", version)) {
    max_version <- get_max_version(data_product, namespace_id)
    patch <- max_version[[1]]$patch
    max_version[[1]]$patch <- as.integer(patch + 1)
    write_version <- as.character(max_version)

    # Increment minor ---------------------------------------------------------

  } else if (grepl("\\$\\{\\{MINOR\\}\\}", version)) {
    max_version <- get_max_version(data_product, namespace_id)
    minor <- max_version[[1]]$minor
    max_version[[1]]$minor <- as.integer(minor + 1)
    write_version <- as.character(max_version)

    # Increment major ---------------------------------------------------------

  } else if (grepl("\\$\\{\\{MAJOR\\}\\}", version)) {
    max_version <- get_max_version(data_product, namespace_id)
    major <- max_version[[1]]$major
    max_version[[1]]$major <- as.integer(major + 1)
    write_version <- as.character(max_version)

  } else if (grepl("\\$\\{\\{DATETIME\\}\\}", version)) {
    datetime <- gsub("-", "", Sys.Date())
    write_version <- gsub("\\$\\{\\{DATETIME\\}\\}", datetime,
                          version)

  } else {
    write_version <- version
  }

  write_version
}
