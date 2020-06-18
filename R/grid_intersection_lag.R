#' grid_intersection
#'
grid_intersection <- function(LAG, gridsize) {
  # Generate grid over bounding box of lowest available geographies shapefile
  n <- gridsize*1000
  grids <- sf::st_make_grid(sf::st_as_sfc(sf::st_bbox(LAG)),
                            cellsize = c(n, n))

  width <- sf::st_bbox(LAG)$xmax - sf::st_bbox(LAG)$xmin
  height <- sf::st_bbox(LAG)$ymax - sf::st_bbox(LAG)$ymin
  num_columns <- ceiling(width / n)
  num_rows <- ceiling(height / n)
  grid_labels <- paste0(1:num_columns, "-", rep(1:num_rows, each = num_columns))
  grid_matrix <- strsplit(grid_labels, "-") %>% lapply(as.numeric) %>% do.call(rbind.data.frame, .)
  colnames(grid_matrix) <- c("x", "y")

  grids <- sf::st_sf(grids, grid_id = grid_labels)

  assertthat::assert_that(max(grid_matrix$x) == num_columns)
  assertthat::assert_that(max(grid_matrix$y) == num_rows)

  lag_subdivisions <- sf::st_intersection(grids, LAG)

  ind <- which(grid_labels %in% unique(lag_subdivisions$grid_id))
  grid_matrix <- grid_matrix[ind,]

  tmp <- sort(apply(grid_matrix, 1, function(x) paste(x[1], x[2], sep = "-")))
  assertthat::assert_that(all(tmp == sort(unique(lag_subdivisions$grid_id))))

  # Use grid to subdivide lowest available geographies
  list(lag_subdivisions = lag_subdivisions,
       grid_matrix = grid_matrix)
}
