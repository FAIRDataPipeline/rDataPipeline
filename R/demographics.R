#' demographics
#'
#' @param h5filename Name of the hd5f file you want to create
#' @param gridsizes vector of lengths (grid sizes), in metres
#' @param ageclasses vector of class numeric corresponding to the lower bound
#' of each age class; when missing, a single age class is generated (all
#' ages combined)
#' @param datazone_sf path to datazone shape file
#' @param postcode_sf path to postcode shape file
#' @param datazone_path path to demographic data file (datazones)
#'
#' @export
#'
demographics <- function(
  h5filename = "scrc_demographics.h5",
  gridsizes = c(10, 1),
  ageclasses = seq(0, 90, 5),
  datazone_sf = "data-raw/shapefiles/SG_DataZone_Bdry_2011.shp",
  postcode_sf = "data-raw/shapefiles/PC_Cut_20_1.shp",
  datazone_path = "data-raw/sape-2018-persons.xlsx") {

  # Create hdf5 file
  rhdf5::h5createFile(h5filename)

  # Create group for scotland_2018
  rhdf5::h5createGroup(h5filename, "scotland_2018")

  # Attach attributes to scotland_2018
  fid <- rhdf5::H5Fopen(h5filename)
  gid <- rhdf5::H5Gopen(fid, name = "scotland_2018/")

  rhdf5::h5writeAttribute(gid, attr = "Population counts for Scottish datazones in 2018. Contains data from ages 0 to 89 in single year increments, as well as 90+.",
                          name = "Description")
  rhdf5::h5writeAttribute(gid, attr = "24/4/20", name = "DownloadDate")
  rhdf5::h5writeAttribute(gid, attr = "1", name = "SourceWarningScore")
  rhdf5::h5writeAttribute(gid, attr = "National Records Scotland", name = "Source")
  rhdf5::h5writeAttribute(gid, attr = "https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/population/population-estimates/2011-based-special-area-population-estimates/small-area-population-estimates/time-series", name = "URL")

  rhdf5::H5Gclose(gid)
  rhdf5::H5Fclose(fid)


  # Attach datazone dataset
  population_dz <- process_population(datazone_path = datazone_path)
  rhdf5::h5write(population_dz, file = h5filename,
                 name = file.path("scotland_2018/datazone"))

  # Attach attributes to the datazone dataset
  fid <- rhdf5::H5Fopen(h5filename)
  did <- rhdf5::H5Dopen(fid, "scotland_2018/datazone")

  rhdf5::h5writeAttribute(did, attr = "population counts", name = "Description")
  rhdf5::h5writeAttribute(did, attr = 1:90, name = "Age classes")
  rhdf5::h5writeAttribute(did, attr = "datazones", name = "Units")
  rhdf5::h5writeAttribute(did, attr = "1", name = "ProcessedWarningScore")

  rhdf5::H5Dclose(did)
  rhdf5::H5Fclose(fid)



  # Initialise shapefiles ---------------------------------------------------

  # Read in datazone shapefile and check for non-intersecting geometries
  datazones <- sf::st_read(datazone_sf) %>% sf::st_make_valid()

  # Read in postcode shapefile
  postcode <- sf::st_read(postcode_sf) %>% sf::st_make_valid()


  # Convert datazones to grids
  for(i in seq_along(gridsizes)) {

    cat(paste0("\nConverting datazones to ", gridsizes[i],
               "k grids using postcode data...\n\n"))

    # Generate grid over bounding box of datazone shapefile
    grids <- sf::st_make_grid(sf::st_as_sfc(sf::st_bbox(datazones)),
                              cellsize = c(gridsizes[i]*1000, gridsizes[i]*1000)) %>%
      sf::st_sf(grid_id = seq_along(.))

    # Use grid to subdivide datazones
    dz_subdivisions <- sf::st_intersection(grids, datazones)

    # Datazones split by postcode; pooled ages
    grid_all_pc <- dz2grid_pc(population_dz = population_dz,
                              datazones = datazones,
                              dz_subdivisions = dz_subdivisions,
                              postcode = postcode)
    tag1 <- paste0("scotland_2018/allages_postcode_", gridsizes[i], "k")
    rhdf5::h5write(grid_all_pc, file = h5filename,
                   name = tag1)

    # Datazones split by postcode; multiple age classes
    grid_groups_pc <- dz2grid_pc(population_dz = population_dz,
                                 ageclasses = ageclasses,
                                 datazones = datazones,
                                 dz_subdivisions = dz_subdivisions,
                                 postcode = postcode)
    tag2 <- paste0("scotland_2018/groupages_postcode_", gridsizes[i], "k")
    rhdf5::h5write(grid_groups_pc, file = h5filename,
                   name = tag2)


    cat(paste0("\nConverting datazones to ", gridsizes[i],
               "k grids using area data...\n\n"))


    # # Transform by area -------------------------------------------------------
    #
    # # Datazones split by area; pooled ages
    # grid_all_area <- dz2grid_area(population_dz = population_dz,
    #                               datazones = datazones,
    #                               dz_subdivisions = dz_subdivisions)
    # tag3 <- paste0("scotland_2018/allages_area_", gridsizes[i], "k")
    # rhdf5::h5write(grid_all_area, file = h5filename,
    #                name = tag3)
    #
    # # Datazones split by area; multiple age classes
    # grid_groups_area <- dz2grid_area(population_dz = population_dz,
    #                                  ageclasses = ageclasses,
    #                                  datazones = datazones,
    #                                  dz_subdivisions = dz_subdivisions)
    # tag4 <- paste0("scotland_2018/groupages_area_", gridsizes[i], "k")
    # rhdf5::h5write(grid_groups_area, file = h5filename,
    #                name = tag4)
    #

    # Attach attributes to the grid datasets
    fid <- rhdf5::H5Fopen(h5filename)
    grid_tag <- paste0(gridsizes[i], "km grid")

    did1 <- rhdf5::H5Dopen(fid, tag1)
    rhdf5::h5writeAttribute(did1, attr = "population counts", name = "Description")
    rhdf5::h5writeAttribute(did1, attr = "pooled", name = "Age classes")
    rhdf5::h5writeAttribute(did1, attr = grid_tag, name = "Units")
    # rhdf5::h5writeAttribute(did1, attr = "postcode", name = "Conversion")
    rhdf5::h5writeAttribute(did1, attr = "1", name = "ProcessedWarningScore")

    did2 <- rhdf5::H5Dopen(fid, tag2)
    rhdf5::h5writeAttribute(did2, attr = "population counts", name = "Description")
    rhdf5::h5writeAttribute(did2, attr = ageclasses, name = "Age classes")
    rhdf5::h5writeAttribute(did2, attr = grid_tag, name = "Units")
    # rhdf5::h5writeAttribute(did2, attr = "postcode", name = "Conversion")
    rhdf5::h5writeAttribute(did2, attr = "1", name = "ProcessedWarningScore")

    # did3 <- rhdf5::H5Dopen(fid, tag3)
    # rhdf5::h5writeAttribute(did3, attr = "population counts", name = "Description")
    # rhdf5::h5writeAttribute(did3, attr = "pooled", name = "Age classes")
    # rhdf5::h5writeAttribute(did3, attr = grid_tag, name = "Units")
    # rhdf5::h5writeAttribute(did3, attr = "area", name = "Conversion")
    # rhdf5::h5writeAttribute(did3, attr = "1", name = "ProcessedWarningScore")
    #
    # did4 <- rhdf5::H5Dopen(fid, tag4)
    # rhdf5::h5writeAttribute(did4, attr = "population counts", name = "Description")
    # rhdf5::h5writeAttribute(did4, attr = ageclasses, name = "Age classes")
    # rhdf5::h5writeAttribute(did4, attr = grid_tag, name = "Units")
    # rhdf5::h5writeAttribute(did4, attr = "area", name = "Conversion")
    # rhdf5::h5writeAttribute(did4, attr = "1", name = "ProcessedWarningScore")

    rhdf5::H5Dclose(did1)
    rhdf5::H5Dclose(did2)
    # rhdf5::H5Dclose(did3)
    # rhdf5::H5Dclose(did4)
    rhdf5::H5Fclose(fid)
  }

}
