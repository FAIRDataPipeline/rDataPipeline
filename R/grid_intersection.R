#' grid_intersection
#'
grid_intersection <- function(datazones, gridsize) {
  # Generate grid over bounding box of datazone shapefile
  n <- gridsize*1000
  grids <- sf::st_make_grid(sf::st_as_sfc(sf::st_bbox(datazones)),
                            cellsize = c(n, n))

  width <- sf::st_bbox(datazones)$xmax - sf::st_bbox(datazones)$xmin
  height <- sf::st_bbox(datazones)$ymax - sf::st_bbox(datazones)$ymin
  num_columns <- ceiling(width / n)
  num_rows <- ceiling(height / n)
  grid_labels <- paste0(rep(1:num_rows, each = num_columns), "-", 1:num_columns)
  grid_matrix <- strsplit(grid_labels, "-") %>% do.call(rbind.data.frame, .)
  colnames(grid_matrix) <- c("y", "x")
  grid_matrix <- select(grid_matrix, x, y)

  grids <- sf::st_sf(grids, grid_id = grid_labels)

  assertthat::assert_that(max(as.numeric(grid_matrix$x)) == num_columns)
  assertthat::assert_that(max(as.numeric(grid_matrix$y)) == num_rows)

  # Use grid to subdivide datazones
  list(dz_subdivisions = sf::st_intersection(grids, datazones),
       grid_matrix = grid_matrix)
}
