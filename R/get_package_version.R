#' Get loaded Package Version
#' @return Returns the current package version
#'
#' @export
#'
get_loaded_package_version <- function(){
  return(utils::packageVersion("SCRCdataAPI"))
}

#' Get github Package Version
#' @return Returns the current package version
#'
#' @export
#'
get_remote_package_version <- function(){
  description <- utils::read.delim("https://raw.githubusercontent.com/ScottishCovidResponse/SCRCdataAPI/master/DESCRIPTION", sep = ":", header = FALSE, row.names = 1)
  if(any(row.names(description) == "Version"))
    return(gsub(" ", "", description['Version',]))
  else
    stop("Something went wrong")
}

#' Check if Package is current version
#'
#' @return Returns true if the loaded package is the same version as the github package
#'
#' @export
#'
is_current_version <- function()
{
    if(as.numeric_version(get_loaded_package_version()) == as.numeric_version(get_remote_package_version()))
      return(TRUE)
    else
      return(FALSE)
}

#' Get startup message
#'
#' @return returns the startup message
#'
#' @export
#'
get_startup_message <- function(){
  if(! is_current_version()){
    return(paste("Warning: Your package version is out of date please update\n", "Git Version: ", get_remote_package_version(), " Local Version: ", get_loaded_package_version()))}
  else{
    if (crayon::has_color())
      return(crayon::green(paste("Version: ", get_loaded_package_version(), " Your package is up to date")))
    else
      return(paste("Version: ", get_loaded_package_version(), " Your package is up to date"))
  }
}
