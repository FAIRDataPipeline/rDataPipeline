#' demographicData
#' 
#' @export
#' 
demographicData <- function(h5filename = "scrc_demographics.h5") {
  # Create hdf5 file
  rhdf5::h5createFile(h5filename)
  
  # Create group for scotland2018 
  rhdf5::h5createGroup(h5filename, "scotland2018")
  
  # Create subgroups for datazone and griddata
  rhdf5::h5createGroup(h5filename, "scotland2018/datazone")
  rhdf5::h5createGroup(h5filename, "scotland2018/griddata")
} 