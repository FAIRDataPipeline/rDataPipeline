#' dz2grid_simd
#' 
#' Converts simd data from datazone to grid format.
#' 
#' @param simd_datazone datazone simd data
#' @param gridsize grid size (length) in metres
#' @param datazone_sf path to datazone shape file 
#' 
dz2grid_simd <- function(simd_datazone, 
                         gridsize,
                         datazone_sf) {
  
  # Read in datazone shapefile and check for non-intersecting geometries
  shape <- sf::st_read(datazone_sf)
  check <- sf::st_is_valid(shape, reason = TRUE)
  if (any(check != "Valid Geometry")) {
    datazones <- sf::st_make_valid(shape)
    assertthat::assert_that(sum(st_area(shape)) == sum(st_area(datazones)))
  } else 
    datazones <- shape
  
  # Datazone-grid conversion -----------------------------------------------
  
  # Generate grid over bounding box of datazone shapefile
  grids <- sf::st_make_grid(sf::st_as_sfc(sf::st_bbox(datazones)), 
                            cellsize = c(gridsize, gridsize)) %>% 
    sf::st_sf(grid_id = seq_along(.))
  
  # Use grid to subdivide datazones
  dz_subdivisions <- sf::st_intersection(grids, datazones) %>% 
    dplyr::rename(datazone = DataZone) %>% 
    data.frame() %>% 
    tibble::remove_rownames()
  
  # Find simd of each datazone subdivision
  simd_dz_subdivisions <- dz_subdivisions %>% 
    dplyr::select(grid_id, datazone) %>% 
    merge(simd_datazone, by = "datazone", all.x = T)
  
  # Find mean across each grid squares
  simd_dz_subdivisions %>% 
    dplyr::group_by(grid_id, .drop = FALSE) %>% 
    dplyr::summarise(mean = mean(simd2020_inc_rate)) %>% 
    dplyr::rename(simd_income = mean)
}