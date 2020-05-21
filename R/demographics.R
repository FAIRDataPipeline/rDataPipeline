#' demographics
#'
#' @param filename Name of output hd5f file
#' @param gridsizes vector of lengths (grid sizes), in metres
#' @param ageclasses vector of class numeric corresponding to the lower bound
#' of each age class; when missing, a single age class is generated (all
#' ages combined)
#' @param datazone_sf path to datazone shape file
#' @param datazone_path path to demographic data file (datazones)
#'
#' @export
#'
demographics <- function(
  filename = "demographics",
  gridsizes = c(10, 1),
  ageclasses = seq(0, 90, 5),
  datazone_sf = "data-raw/shapefiles/SG_DataZone_Bdry_2011.shp",
  datazone_path = "data-raw/sape-2018-persons.xlsx") {

  # Create hdf5 file
  h5filename <- paste0(filename, ".h5")
  rhdf5::h5createFile(h5filename)

  # Create group for scotland_2018
  rhdf5::h5createGroup(h5filename, "scotland_2018")

  # Attach attributes to scotland_2018
  fid <- rhdf5::H5Fopen(h5filename)
  gid <- rhdf5::H5Gopen(fid, name = "scotland_2018/")

  rhdf5::h5writeAttribute(gid, attr = "Population counts for Scottish datazones in 2018. Contains data from ages 0 to 89 in single year increments, as well as 90+.",
                          name = "Description")
  rhdf5::h5writeAttribute(gid, attr = "24/4/20", name = "DownloadDate")
  rhdf5::h5writeAttribute(gid, attr = "0", name = "SourceWarningScore")
  rhdf5::h5writeAttribute(gid, attr = "National Records Scotland", name = "Source")
  rhdf5::h5writeAttribute(gid, attr = "https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/population/population-estimates/2011-based-special-area-population-estimates/small-area-population-estimates/time-series", name = "datazone_dat")
  rhdf5::h5writeAttribute(gid, attr = "https://data.gov.uk/dataset/ab9f1f20-3b7f-4efa-9bd2-239acf63b540/data-zone-boundaries-2011", name = "datazone_sf")

  rhdf5::H5Gclose(gid)
  rhdf5::H5Fclose(fid)


  # Attach datazone dataset
  population_dz <- process_population(datazone_path = datazone_path)
  rhdf5::h5write(population_dz[-1], file = h5filename,
                 name = file.path("scotland_2018/datazone_raw"))
  rhdf5::h5write(population_dz$datazone, file = h5filename,
                 name = file.path("scotland_2018/datazone_id"))

  # Attach attributes to the datazone dataset
  fid <- rhdf5::H5Fopen(h5filename)
  did <- rhdf5::H5Dopen(fid, "scotland_2018/datazone_raw")

  rhdf5::h5writeAttribute(did, attr = "population counts (original spatial resolution)", name = "Description")
  age_tag <- 1:90
  age_tag[length(age_tag)] <- paste0(age_tag[length(age_tag)], "+")
  rhdf5::h5writeAttribute(did, attr = age_tag, name = "Age classes")
  rhdf5::h5writeAttribute(did, attr = "datazones", name = "Units")
  rhdf5::h5writeAttribute(did, attr = "0", name = "ProcessedWarningScore")

  rhdf5::H5Dclose(did)
  rhdf5::H5Fclose(fid)


  # Read in datazone shapefile and check for non-intersecting geometries
  datazones <- sf::st_read(datazone_sf, quiet = TRUE) %>% sf::st_make_valid()

  # Convert datazones to grids
  for(i in seq_along(gridsizes)) {

    cat(paste0("\nConverting datazones to ", gridsizes[i],
               "k grids...\n\n"))
    cat("Initialising...\n\n")

    # Generate grid over bounding box of datazone shapefile
    n <- gridsizes[i]*1000
    grids <- sf::st_make_grid(sf::st_as_sfc(sf::st_bbox(datazones)),
                              cellsize = c(n, n))

    width <- sf::st_bbox(datazones)$xmax - sf::st_bbox(datazones)$xmin
    height <- sf::st_bbox(datazones)$ymax - sf::st_bbox(datazones)$ymin
    num_columns <- ceiling(width / n)
    num_rows <- ceiling(height / n)
    grid_labels <- paste0(1:num_rows, "-", rep(1:num_columns, each = num_rows))

    # grids <- sf::st_sf(grids, grid_id = seq_along(grids))
    grids <- sf::st_sf(grids, grid_id = grid_labels)

    # Use grid to subdivide datazones
    dz_subdivisions <- sf::st_intersection(grids, datazones)

    # Datazones split by area; total population
    grid_all <- dz2grid_area(population_dz = population_dz,
                             datazones = datazones,
                             dz_subdivisions = dz_subdivisions)
    grid_all_tag <- paste0("scotland_2018/grid", gridsizes[i], "k_total")
    rhdf5::h5write(grid_all$grid_pop, file = h5filename,
                   name = grid_all_tag)

    # Datazones split by area; binned age classes
    grid_groups <- dz2grid_area(population_dz = population_dz,
                                ageclasses = ageclasses,
                                datazones = datazones,
                                dz_subdivisions = dz_subdivisions)
    grid_groups_tag <- paste0("scotland_2018/grid", gridsizes[i], "k_binned")
    rhdf5::h5write(grid_groups$grid_pop, file = h5filename,
                   name = grid_groups_tag)

    assertthat::assert_that(all(grid_all$grid_id == grid_groups$grid_id))

    grid_id_tag <- paste0("scotland_2018/grid", gridsizes[i], "k_id")
    rhdf5::h5write(grid_all$grid_id, file = h5filename,
                   name = grid_id_tag)


    # Attach attributes to the grid datasets
    fid <- rhdf5::H5Fopen(h5filename)
    grid_tag <- paste0(gridsizes[i], "km grid")

    didall <- rhdf5::H5Dopen(fid, grid_all_tag)
    rhdf5::h5writeAttribute(didall, attr = "population counts", name = "Description")
    age_tag <- paste0(min(ageclasses), "-", max(ageclasses), "+")
    rhdf5::h5writeAttribute(didall, attr = age_tag, name = "Age classes")
    rhdf5::h5writeAttribute(didall, attr = grid_tag, name = "Units")
    rhdf5::h5writeAttribute(didall, attr = "2", name = "ProcessedWarningScore")

    didgrp <- rhdf5::H5Dopen(fid, grid_groups_tag)
    rhdf5::h5writeAttribute(didgrp, attr = "population counts", name = "Description")
    age_tag <- sapply(seq_along(ageclasses), function(x) {
      if(x != length(ageclasses)) {
        paste0(ageclasses[x], "-", ageclasses[x+1]-1)
      } else
        paste0(ageclasses[x], "+")
    })
    rhdf5::h5writeAttribute(didgrp, attr = age_tag, name = "Age classes")
    rhdf5::h5writeAttribute(didgrp, attr = grid_tag, name = "Units")
    rhdf5::h5writeAttribute(didgrp, attr = "2", name = "ProcessedWarningScore")

    rhdf5::H5Dclose(didall)
    rhdf5::H5Dclose(didgrp)
    rhdf5::H5Fclose(fid)
  }

}
