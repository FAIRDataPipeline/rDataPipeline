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
  grid_labels <- paste0(1:num_columns, "-", rep(1:num_rows, each = num_columns))
  grid_matrix <- strsplit(grid_labels, "-") %>% do.call(rbind.data.frame, .)
  colnames(grid_matrix) <- c("x", "y")
  grid_matrix <- grid_matrix %>%
    dplyr::mutate_if(is.character, as.numeric)

  grids <- sf::st_sf(grids, grid_id = grid_labels)

  assertthat::assert_that(max(as.numeric(grid_matrix$x)) == num_columns)
  assertthat::assert_that(max(as.numeric(grid_matrix$y)) == num_rows)

  dz_subdivisions <- sf::st_intersection(grids, datazones)

  ind <- which(grid_labels %in% unique(dz_subdivisions$grid_id))
  grid_matrix <- grid_matrix[ind,]

  tmp <- sort(apply(grid_matrix, 1, function(x) paste(x[1], x[2], sep = "-")))
  assertthat::assert_that(all(tmp == sort(unique(dz_subdivisions$grid_id))))

  # Use grid to subdivide datazones
  list(dz_subdivisions = dz_subdivisions,
       grid_matrix = grid_matrix)
}
