check_for_hdf5<-function(filename, component){
  # Does file exist?
  if(file.exists(filename)==TRUE){
    # If yes:
    # Does the component exist?
    file.h5 <- H5File$new(filename, mode = "r")
    check=any(grepl(component,file.h5$ls(recursive=TRUE)$name))
    file.h5$close_all()
  }else if(file.exists(filename)==FALSE){
    # If no:
    check=FALSE
  }
  check
}
