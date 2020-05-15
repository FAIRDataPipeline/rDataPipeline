#' demographicData
#' 
#' @param h5filename Name of the hd5f file you want to create
#' @param datazone_path path to demographic data file (datazones)
#' @param simd_path path to simd data file 
#' 
#' @export
#' 
demographicData <- function(
  h5filename = "scrc_demographics.h5",
  datazone_path = "data-raw/population_datazone/sape-2018-persons.xlsx",
  simd_path = "data-raw/DataZone2011_simd2020.csv") 
{
  # Initialise hdf5 file ----------------------------------------------------
  
  # Create hdf5 file
  rhdf5::h5createFile(h5filename)
  
  # Create groups for each dataset
  rhdf5::h5createGroup(h5filename, "scotland_2018")
  rhdf5::h5createGroup(h5filename, "simd_income")

  
  # Population data --------------------------------------------------------
  
  # Process data and write to group
  sape_persons <- process_population(datazone_path)
  rhdf5::h5write(sape_persons, file = h5filename, 
                 name = file.path("scotland_2018/datazone"))
  
  # Add attributes / metadata 
  fid <- rhdf5::H5Fopen(h5filename)
  
  gid <- rhdf5::H5Gopen(fid, name = "scotland_2018/") 
  rhdf5::h5writeAttribute(gid, attr = "Population of datazones in Scotland in 2018 contains data for single year of age from age 0 to 89 and a 90+ age class and all ages. Processed to remove unnecessary identifier columns/rows.", 
                          name = "Description")
  rhdf5::h5writeAttribute(gid, attr = "24/4/20", name = "DownloadDate")
  rhdf5::h5writeAttribute(gid, attr = "1", name = "RawWarningScore")
  rhdf5::h5writeAttribute(gid, attr = "National Records Scotland", name = "Source")
  rhdf5::h5writeAttribute(gid, attr = "https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/population/population-estimates/2011-based-special-area-population-estimates/small-area-population-estimates/time-series", name = "URL")
  
  did <- rhdf5::H5Dopen(fid, "scotland_2018/datazone")
  h5writeAttribute(did, attr = "1", name = "ProcessedWarningScore")
  
  rhdf5::H5Dclose(did)
  rhdf5::H5Gclose(gid)
  rhdf5::H5Fclose(fid)
  
  
  # SIMD data --------------------------------------------------------------
  
  # Process data
  simd_income <- process_simd(simd_path)
  h5write(simd_income, h5filename,
          name = "simd_income/datazone")
  
  # Add attributes / metadata 
  fid <- rhdf5::H5Fopen(h5filename)
  
  gid <- rhdf5::H5Gopen(fid, name = "simd_income/") 
  rhdf5::h5writeAttribute(gid, attr = "24/4/20", name = "DownloadDate")
  rhdf5::h5writeAttribute(gid, attr = "1", name = "RawWarningScore")
  
  did <- rhdf5::H5Dopen(fid, "simd_income/datazone")
  h5writeAttribute(did, attr = "1", name = "ProcessedWarningScore")
  
  rhdf5::H5Dclose(did)
  rhdf5::H5Gclose(gid)
  rhdf5::H5Fclose(fid)
  
} 