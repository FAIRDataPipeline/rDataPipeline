#' Check if Package is current version
#'
#' @return Returns true if the loaded package is the same version as the github
#' package
#'
is_current_version <- function()
{
  if(as.numeric_version(get_loaded_package_version()) ==
     as.numeric_version(get_remote_package_version()))
    return(TRUE)
  else
    return(FALSE)
}
