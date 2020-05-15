#' demographicData
#' 
#' @param h5filename Name of the hd5f file you want to create
#' @param path path to demographic data input file (datazones)
#' 
#' @export
#' 
demographicData <- function(h5filename = "scrc_demographics.h5",
                            path = "data-raw/population_datazone/sape-2018-persons.xlsx"
) {
  
  # Initialise hdf5 file ----------------------------------------------------
  
  # Create hdf5 file
  rhdf5::h5createFile(h5filename)
  
  # Create group for scotland2018 
  rhdf5::h5createGroup(h5filename, "scotland2018")
  
  # Create subgroups for datazone and griddata
  rhdf5::h5createGroup(h5filename, "scotland2018/datazone")
  rhdf5::h5createGroup(h5filename, "scotland2018/griddata")
  
  
  # Process data ------------------------------------------------------------
  
  sape_persons <- process_population(path)
  
  rhdf5::h5write(sape_persons, file = h5filename, 
                 name = file.path("scotland2018/datazone"))
  
  
  # Add attributes / metadata -----------------------------------------------
  
  # Open file
  fid <- rhdf5::H5Fopen(h5filename)
  
  # Add some attributes to the group
  gid <- rhdf5::H5Gopen(fid, name = "scotland2018/") 
  
  rhdf5::h5writeAttribute(did, attr = "Population of datazones in Scotland in 2018 contains data for single year of age from age 0 to 89 and a 90+ age class and all ages. Processed to remove unnecessary identifier columns/rows.", 
                          name = "Description")
  rhdf5::h5writeAttribute(did, attr = "24/4/20", name = "DownloadDate")
  rhdf5::h5writeAttribute(did, attr = "1", name = "RawWarningScore")
  rhdf5::h5writeAttribute(did, attr = "National Records Scotland", name = "Source")
  rhdf5::h5writeAttribute(did, attr = "https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/population/population-estimates/2011-based-special-area-population-estimates/small-area-population-estimates/time-series", name = "URL")
  
  # Add some attributes to the dataset (datazone)
  did <- rhdf5::H5Dopen(fid, "scotland2018/datazone")
  
  h5writeAttribute(did, attr = "1", name = "ProcessedWarningScore")
  
  # Close file, groups, and datasets
  rhdf5::H5Fclose(fid)
  rhdf5::H5Fclose(gid)
  rhdf5::H5Dclose(did)
} 