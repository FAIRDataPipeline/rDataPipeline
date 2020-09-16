#' increment_version
#'
#' Generate version number by pulling all version numbers from the data registry
#' and incrementing patch by 1. Otherwise, if a version number is input, checks
#' are made to determine whether version number is valid.
#'
#' @param data_product a \code{string} specifying the name of the data product
#' @param version a \code{string} specifying the version number
#'
#' @export
#'
increment_version <- function(data_product, version) {
  tmp <- get_entry("data_product", list(name = data_product))
  versions <- lapply(tmp, function(x) x$version) %>% unlist()
  max_version <- max(versions) %>% semver::parse_version()

  # If version is missing, increment patch by one
  if(missing(version)) {
    semver::increment_version(x = max_version, field = "patch", value = 1L)

  } else {
    if(version < max_version)
      warning(paste0("The data registry already contains ", data_product,
                    " v", max_version, " (a higher version number)"))
    if(version == max_version)
      warning(paste0("The data registry already contains ", data_product,
                     " v", max_version, " (the same version number)"))
  }

}
