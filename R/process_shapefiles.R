#' process_shapefiles
#' 
#' @param gridsizes vector of lengths (grid sizes), in metres
#' @param datazone_sf path to datazone shape file 
#' @param postcode_sf path to postcode shape file 
#' 
process_shapefiles <- function(gridsize,
                               datazone_sf,
                               postcode_sf) {
  
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
  
  list(datazones = datazones,
       grids = grids,
       dz_subdivisions = dz_subdivisions,
       postcode = postcode)
}