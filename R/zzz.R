.onLoad <- function(...){
  # Check package is up to date
  tryCatch({
    if(! is_current_version())
      message("Warning: Your package version is out of date please update")
    else
      message("Your package is up to date")
  }, error = function(e){
    message("Could not check if updates are available, please check manually")
  }, message = function (m){
    message(m)
  })
}
