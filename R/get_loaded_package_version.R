#' Get loaded Package Version
#'
#' @param package package name
#'
#' @return Returns the current package version
#'
get_loaded_package_version <- function(package = "SCRCdataAPI"){
  utils::packageVersion(package)
}
