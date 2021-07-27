#' Get loaded package version
#'
#' @param package a \code{string} specifying the package name
#'
#' @return Returns the current package version
#'
get_loaded_package_version <- function(package = "SCRCdataAPI"){
  utils::packageVersion(package)
}
