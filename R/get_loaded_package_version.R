#' Get loaded Package Version
#'
#' @return Returns the current package version
#'
#' @export
#'
get_loaded_package_version <- function(package = "SCRCdataAPI"){
  return(utils::packageVersion(package))
}
