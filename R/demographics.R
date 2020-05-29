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
  file.h5 <- H5File$new(h5filename, mode = "w")

  # Create groups
  scotland_2018.grp <- file.h5$create_group("scotland_2018")


  # Attach attributes to scotland_2018
  hdf5r::h5attr(scotland_2018.grp, "Description") <- "Population counts for Scottish datazones in 2018. Contains data from ages 0 to 89 in single year increments, as well as 90+."
  hdf5r::h5attr(scotland_2018.grp, "DownloadDate") <- as.character(Sys.time())
  hdf5r::h5attr(scotland_2018.grp, "SourceWarningScore") <- "0"
  hdf5r::h5attr(scotland_2018.grp, "Source") <- "National Records Scotland"
  hdf5r::h5attr(scotland_2018.grp, "datazone_dat") <- "https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/population/population-estimates/2011-based-special-area-population-estimates/small-area-population-estimates/time-series"
  hdf5r::h5attr(scotland_2018.grp, "datazone_sf") <- "https://data.gov.uk/dataset/ab9f1f20-3b7f-4efa-9bd2-239acf63b540/data-zone-boundaries-2011"


  # Attach datazone dataset
  population_dz <- process_population(datazone_path = datazone_path)
  scotland_2018.grp[["datazone_id"]] <- population_dz$datazone
  scotland_2018.grp[["datazone_raw"]] <- as.matrix(population_dz[-1])

  # Attach datazone attributes
  hdf5r::h5attr(scotland_2018.grp[["datazone_raw"]], "Description") <- "Population counts (original spatial resolution)"
  hdf5r::h5attr(scotland_2018.grp[["datazone_raw"]], "Age classes") <- gsub("AGE", "", colnames(population_dz)[-1])
  hdf5r::h5attr(scotland_2018.grp[["datazone_raw"]], "Units") <- "datazones"
  hdf5r::h5attr(scotland_2018.grp[["datazone_raw"]], "ProcessedWarningScore") <- "0"


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
    grid_matrix <- cbind.data.frame(x = 1:num_rows,
                                    y = rep(1:num_columns, each = num_rows))

    grids <- sf::st_sf(grids, grid_id = grid_labels)

    # Use grid to subdivide datazones
    dz_subdivisions <- sf::st_intersection(grids, datazones)

    datazone_pop <- bin_ages(ageclasses = ageclasses,
                             population_dz = population_dz)

    # Datazones split by area; total population
    grid_all <- dz2grid(dat = datazone_pop,
                        datazones = datazones,
                        dz_subdivisions = dz_subdivisions)

    # Add to hdf5
    grid_all_tag <- paste0("grid", gridsizes[i], "k_total")
    scotland_2018.grp[[grid_all_tag]] <- grid_all$grid_pop

    grid_tag <- paste0(gridsizes[i], "km grid")
    hdf5r::h5attr(scotland_2018.grp[[grid_all_tag]],
                  "Description") <-
      "Population counts"
    age_tag <- paste0(min(ageclasses), "-", max(ageclasses), "+")
    hdf5r::h5attr(scotland_2018.grp[[grid_all_tag]],
                  "Age classes") <- age_tag
    hdf5r::h5attr(scotland_2018.grp[[grid_all_tag]],
                  "Units") <- grid_tag
    hdf5r::h5attr(scotland_2018.grp[[grid_all_tag]],
                  "ProcessedWarningScore") <- "2"


    # Datazones split by area; binned age classes
    grid_groups <- dz2grid(dat = datazone_pop,
                           datazones = datazones,
                           dz_subdivisions = dz_subdivisions)

    # Add to hdf5
    grid_groups_tag <- paste0("grid", gridsizes[i], "k_binned")
    scotland_2018.grp[[grid_groups_tag]] <- grid_groups$grid_pop

    assertthat::assert_that(all(grid_all$grid_id == grid_groups$grid_id))
    grid_id_tag <- paste0("grid", gridsizes[i], "k_id")
    scotland_2018.grp[[grid_id_tag]] <- grid_matrix

    age_tag <- sapply(seq_along(ageclasses), function(x) {
      if(x != length(ageclasses)) {
        paste0(ageclasses[x], "-", ageclasses[x+1]-1)
      } else
        paste0(ageclasses[x], "+")
    })

    hdf5r::h5attr(scotland_2018.grp[[grid_groups_tag]],
                  "Description") <- "Population counts"
    hdf5r::h5attr(scotland_2018.grp[[grid_groups_tag]],
                  "Age classes") <- age_tag
    hdf5r::h5attr(scotland_2018.grp[[grid_groups_tag]],
                  "Units") <- grid_tag
    hdf5r::h5attr(scotland_2018.grp[[grid_groups_tag]],
                  "ProcessedWarningScore") <- "2"

  }
  file.h5$close_all()

}
