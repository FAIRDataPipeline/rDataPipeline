#' Get loaded Package Version
#' @return Returns the current package version
#'
#' @export
#'
get_loaded_package_version <- function(){
  return(packageVersion("SCRCdataAPI"))
}

#' Get github Package Version
#' @return Returns the current package version
#'
#' @export
#'
get_remote_package_version <- function(){
  description <- read.delim("https://raw.githubusercontent.com/ScottishCovidResponse/SCRCdataAPI/master/DESCRIPTION", sep = ":", header = FALSE, row.names = 1)
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
