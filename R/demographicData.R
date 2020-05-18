#' demographicData
#' 
#' @param h5filename Name of the hd5f file you want to create
#' @param datazone_path path to demographic data file (datazones)
#' @param simd_path path to simd data file 
#' @param datazone_sf path to datazone shape file 
#' @param postcode_sf path to postcode shape file 
#' 
#' @export
#' 
demographicData <- function(
  h5filename = "scrc_demographics.h5",
  datazone_path = "data-raw/population_datazone/sape-2018-persons.xlsx",
  simd_path = "data-raw/DataZone2011_simd2020.csv",
  datazone_sf = "data-raw/shapefiles/SG_DataZone_Bdry_2011.shp",
  postcode_sf = "data-raw/shapefiles/PC_Cut_20_1.shp") 
{
  # Initialise hdf5 file ----------------------------------------------------
  
  # Create hdf5 file
  rhdf5::h5createFile(h5filename)
  
  # Create groups for each dataset
  rhdf5::h5createGroup(h5filename, "scotland_2018")
  rhdf5::h5createGroup(h5filename, "simd_income")
  
  
  # Population data (datazones) ---------------------------------------------
  
  # Process data and write to group
  population_dz <- process_population(datazone_path = datazone_path)
  rhdf5::h5write(population_dz, file = h5filename, 
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
  
  
  # Population data (grids) -------------------------------------------------
  
  population_grid_allages <- dz2grid_pop(population_dz = population_dz, 
                                         datazone_sf = datazone_sf, 
                                         postcode_sf = postcode_sf, 
                                         grid_size = grid_size)
  h5write(population_grid_allages, file = h5filename, 
          name = "scotland_2018/grid_10km_allages")
  
  population_grid_agegroups <- dz2grid_pop(population_dz = population_dz, 
                                           datazone_sf = datazone_sf, 
                                           postcode_sf = postcode_sf, 
                                           grid_size = grid_size,
                                           ageclasses = seq(0, 90, 5))
  h5write(population_grid_agegroups, file = h5filename, 
          name = "scotland_2018/grid_10km_agegroups")
  
  
  # SIMD (datazones) -------------------------------------------------------
  
  # Process data
  simd_datazone <- process_simd(simd_path = simd_path)
  h5write(simd_datazone, h5filename, name = "simd_income/datazone")
  
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
  
  
  # SIMD (grids) -----------------------------------------------------------
  
  simd_grid <- dz2grid_simd(simd_datazone = simd_datazone, 
                            datazone_sf = datazone_sf, 
                            grid_size = grid_size)
  rhdf5::h5write(simd_grid, file = h5filename,
                 name = "simd_income/grid_10km")
  
} 