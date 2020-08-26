#' check_for_hdf5
#' internal function to check if a component is in a given hdf5 file
#'
#' @param filename file path to the hdf5
#' @param component component name to check for
#'
#'
check_for_hdf5 <- function(filename, component){

  #Default value for check
  check <- FALSE

  if(is.null(filename) | is.na(filename))  {
   stop("filename should not be NULL or NA")
  }
  if(file.exists(filename)==TRUE){
    # If yes:
    # Does the component exist?
    file.h5 <- H5File$new(filename, mode = "r")
    check <- any(grepl(component,file.h5$ls(recursive=TRUE)$name))
    file.h5$close_all()
    }

  check
}
