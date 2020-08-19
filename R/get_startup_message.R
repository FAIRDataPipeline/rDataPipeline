#' Get startup message
#'
#' @return returns the startup message
#'
get_startup_message <- function(){
  if(! is_current_version()){
    return(paste("Warning: Your package version is out of date please update\n",
                 "Git Version: ", get_remote_package_version(),
                 " Local Version: ", get_loaded_package_version()))}
  else{
    if (crayon::has_color())
      return(crayon::green(paste("Version: ", get_loaded_package_version(),
                                 " Your package is up to date")))
    else
      return(paste("Version: ", get_loaded_package_version(),
                   " Your package is up to date"))
  }
}
