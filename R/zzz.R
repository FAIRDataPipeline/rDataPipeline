.onAttach <- function(...){
  # Check package is up to date
  tryCatch({
    packageStartupMessage(get_startup_message())
  }, error = function(e){
    packageStartupMessage("Could not check if updates are available, please check manually")
  })
}
