.onLoad <- function(...){
  # Check package is up to date
  tryCatch({
    if(! is_current_version()){
      message(paste("Warning: Your package version is out of date please update\n", "Git Version: ", get_remote_package_version(), " Local Version: ", get_loaded_package_version()))}
    else{
      if (crayon::has_color())
        message(crayon::green(paste("Version: ", get_loaded_package_version(), " Your package is up to date")))
      else
        message(paste("Version: ", get_loaded_package_version(), " Your package is up to date"))
    }

  }, error = function(e){
    message("Could not check if updates are available, please check manually")
  }, message = function (m){
    message(m)
  })
}
