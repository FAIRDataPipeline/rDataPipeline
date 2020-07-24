delete_hdf5_link = function(filename, component){
  file.h5 <- H5File$new(filename, mode = "r")
  file.h5$link_delete(component)
  file.h5$close_all()
}