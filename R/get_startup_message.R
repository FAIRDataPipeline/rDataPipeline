#' Get startup message
#'
#' @param repo a \code{string} specifying the github username/repository
#' @param package a \code{string} specifying the package name
#'
#' @return returns the startup message
#'
#' @examples
#' \dontrun{
#' get_startup_message("ScottishCovidResponse/SCRCdata", "SCRCdata")
#' }
#'
get_startup_message <- function(repo, package){
  if(!is_current_version(repo, package)){
    return(paste("Warning: Your package version is out of date please update\n",
                 "Git Version: ", get_remote_package_version(repo),
                 " Local Version: ", get_loaded_package_version(package)))}
  else{
    if (crayon::has_color())
      return(crayon::green(paste("Version: ", get_loaded_package_version(package),
                                 " Your package is up to date")))
    else
      return(paste("Version: ", get_loaded_package_version(package),
                   " Your package is up to date"))
  }
}
