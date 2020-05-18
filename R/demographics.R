#' demographics
#' 
#' @param h5filename Name of the hd5f file you want to create
#' @param gridsize grid size (length) in metres
#' @param ageclasses vector of class numeric corresponding to the lower bound
#' of each age class; when missing, a single age class is generated (all 
#' ages combined)
#' @param datazone_sf path to datazone shape file 
#' @param postcode_sf path to postcode shape file 
#' @param datazone_path path to demographic data file (datazones)
#' @param simd_path path to simd data file 
#' 
#' @export
#' 
demographics <- function(
  h5filename = "scrc_demographics.h5",
  gridsize = 10,
  ageclasses = seq(0, 90, 5),
  datazone_sf = "data-raw/shapefiles/SG_DataZone_Bdry_2011.shp",
  postcode_sf = "data-raw/shapefiles/PC_Cut_20_1.shp",
  datazone_path = "data-raw/sape-2018-persons.xlsx",
  simd_path = "data-raw/DataZone2011_simd2020.csv") {
  # Initialise hdf5 file ----------------------------------------------------
  
  # Create hdf5 file
  rhdf5::h5createFile(h5filename)
  
  # Create groups for each dataset
  rhdf5::h5createGroup(h5filename, "scotland_2018")
  rhdf5::h5createGroup(h5filename, "simd_income")
  
  
  # Population data (datazones) ---------------------------------------------
  
  cat("\nGenerating population data...\n")
  
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
  rhdf5::h5writeAttribute(did, attr = "Datazone population counts", 
                          name = "Description")
  rhdf5::h5writeAttribute(did, attr = "1", name = "ProcessedWarningScore")
  
  rhdf5::H5Dclose(did)
  rhdf5::H5Gclose(gid)
  rhdf5::H5Fclose(fid)
  
  
  # Population data (grids) -------------------------------------------------
  
  cat("\nConverting datazones to grids using postcode data...\n")
  
  # Read in datazone shapefile and check for non-intersecting geometries
  datazones <- sf::st_read(datazone_sf) %>% sf::st_make_valid()
  
  # Generate grid over bounding box of datazone shapefile
  grids <- sf::st_make_grid(sf::st_as_sfc(sf::st_bbox(datazones)), 
                            cellsize = c(gridsize*1000, gridsize*1000)) %>% 
    sf::st_sf(grid_id = seq_along(.))
  
  # Use grid to subdivide datazones
  dz_subdivisions <- sf::st_intersection(grids, datazones)
  
  # Read in postcode shapefile 
  postcode <- sf::st_read(postcode_sf) %>% sf::st_make_valid()
  
  
  # Datazones split by postcode; all ages combined
  grid_all_pc <- dz2grid_pop(population_dz = population_dz, 
                             method = "postcode",
                             dz_subdivisions = dz_subdivisions, 
                             postcode = postcode)
  tag1 <- paste0("scotland_2018/allages_postcode_", gridsize, "k")
  rhdf5::h5write(grid_all_pc, file = h5filename, 
                 name = tag1)
  
  # Datazones split by postcode; multiple age classes
  grid_groups_pc <- dz2grid_pop(population_dz = population_dz, 
                                method = "postcode",
                                ageclasses = ageclasses,
                                dz_subdivisions = dz_subdivisions, 
                                postcode = postcode)
  tag2 <- paste0("scotland_2018/groupages_postcode_", gridsize, "k")
  rhdf5::h5write(grid_groups_pc, file = h5filename, 
                 name = tag2)
  
  
  cat("\nConverting datazones to grids using area data...\n")
  
  # Datazones split by area; all ages combined
  grid_all_area <- dz2grid_pop(population_dz = population_dz, 
                               method = "area",
                               datazones = datazones,
                               dz_subdivisions = dz_subdivisions, 
                               postcode = postcode)
  tag3 <- paste0("scotland_2018/allages_area_", gridsize, "k")
  rhdf5::h5write(grid_all_area, file = h5filename, 
                 name = tag3)
  
  # Datazones split by area; multiple age classes
  grid_groups_area <- dz2grid_pop(population_dz = population_dz,
                                  method = "area",
                                  ageclasses = ageclasses,
                                  datazones = datazones,
                                  dz_subdivisions = dz_subdivisions, 
                                  postcode = postcode)
  tag4 <- paste0("scotland_2018/groupages_area_", gridsize, "k")
  rhdf5::h5write(grid_groups_area, file = h5filename, 
                 name = tag4)
  
  assertthat::assert_that(sum(rowSums(population_dz[-1])) ==
                            unique(c(sum(rowSums(grid_all_pc[-1])),
                                     sum(rowSums(grid_groups_pc[-1])),
                                     sum(rowSums(grid_all_area[-1])),
                                     sum(rowSums(grid_groups_area[-1])))))
  
  # Add attributes / metadata 
  fid <- rhdf5::H5Fopen(h5filename)
  
  did1 <- rhdf5::H5Dopen(fid, tag1)
  txt <- paste0(gridsize, " km grid population counts")
  rhdf5::h5writeAttribute(did1, attr = txt, name = "Description")
  rhdf5::h5writeAttribute(did1, attr = "1", name = "ProcessedWarningScore")
  
  did2 <- rhdf5::H5Dopen(fid, tag2)
  rhdf5::h5writeAttribute(did1, attr = "Datazone population counts", 
                          name = "Description")
  rhdf5::h5writeAttribute(did1, attr = "1", name = "ProcessedWarningScore")
  
  did3 <- rhdf5::H5Dopen(fid, tag2)
  rhdf5::h5writeAttribute(did1, attr = "Datazone population counts", 
                          name = "Description")
  rhdf5::h5writeAttribute(did1, attr = "1", name = "ProcessedWarningScore")
  
  did4 <- rhdf5::H5Dopen(fid, tag2)
  rhdf5::h5writeAttribute(did1, attr = "Datazone population counts", 
                          name = "Description")
  rhdf5::h5writeAttribute(did1, attr = "1", name = "ProcessedWarningScore")
  
  rhdf5::H5Dclose(did1)
  rhdf5::H5Dclose(did2)
  rhdf5::H5Dclose(did3)
  rhdf5::H5Dclose(did4)
  rhdf5::H5Fclose(fid)
  
  
  # SIMD (datazones) -------------------------------------------------------
  
  cat("\nGenerating SIMD data...\n")
  
  # Process data
  simd_datazone <- process_simd(simd_path = simd_path)
  rhdf5::h5write(simd_datazone, h5filename, name = "simd_income/datazone")
  
  # Add attributes / metadata 
  fid <- rhdf5::H5Fopen(h5filename)
  
  gid <- rhdf5::H5Gopen(fid, name = "simd_income/") 
  rhdf5::h5writeAttribute(gid, attr = "24/4/20", name = "DownloadDate")
  rhdf5::h5writeAttribute(gid, attr = "1", name = "RawWarningScore")
  
  did <- rhdf5::H5Dopen(fid, "simd_income/datazone")
  rhdf5::h5writeAttribute(did, attr = "1", name = "ProcessedWarningScore")
  
  rhdf5::H5Dclose(did)
  rhdf5::H5Gclose(gid)
  rhdf5::H5Fclose(fid)
  
  
  # SIMD (grids) -----------------------------------------------------------
  
  cat("\nConverting datazones to grids...\n")
  
  simd_grid <- dz2grid_simd(simd_datazone = simd_datazone, 
                            gridsize = gridsize*1000,
                            datazone_sf = datazone_sf)
  tag <- paste0("simd_income/grid_", gridsize, "k")
  rhdf5::h5write(simd_grid, file = h5filename,
                 name = tag)
  
} 
