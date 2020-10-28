#' Check if Package is current version
#'
#' @param repo repo name
#' @param package package name
#'
#' @return Returns true if the loaded package is the same version as the github
#' package
#' @keywords internal
#'
is_current_version <- function(repo = "ScottishCovidResponse/SCRCdataAPI",
                               package = "SCRCdataAPI")
{
  if(as.numeric_version(get_loaded_package_version(package)) ==
     as.numeric_version(get_remote_package_version(repo)))
    return(TRUE)
  else
    return(FALSE)
}
