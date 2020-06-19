#' grid_intersection
#'
grid_intersection <- function(shapefile, gridsize) {
  # Generate grid over bounding box of datazone shapefile
  n <- gridsize*1000
  grids <- sf::st_make_grid(sf::st_as_sfc(sf::st_bbox(shapefile)),
                            cellsize = c(n, n))

  width <- sf::st_bbox(shapefile)$xmax - sf::st_bbox(shapefile)$xmin
  height <- sf::st_bbox(shapefile)$ymax - sf::st_bbox(shapefile)$ymin
  num_columns <- ceiling(width / n)
  num_rows <- ceiling(height / n)
  grid_labels <- paste0(1:num_columns, "-", rep(1:num_rows, each = num_columns))
  grid_matrix <- strsplit(grid_labels, "-") %>% lapply(as.numeric) %>% do.call(rbind.data.frame, .)
  colnames(grid_matrix) <- c("x", "y")

  grids <- sf::st_sf(grids, grid_id = grid_labels)

  assertthat::assert_that(max(grid_matrix$x) == num_columns)
  assertthat::assert_that(max(grid_matrix$y) == num_rows)

  subdivisions <- sf::st_intersection(grids, shapefile)

  ind <- which(grid_labels %in% unique(subdivisions$grid_id))
  grid_matrix <- grid_matrix[ind,]

  tmp <- sort(apply(grid_matrix, 1, function(x) paste(x[1], x[2], sep = "-")))
  assertthat::assert_that(all(tmp == sort(unique(subdivisions$grid_id))))

  # Use grid to subdivide shapefile
  list(subdivisions = subdivisions,
       grid_matrix = grid_matrix)
}
