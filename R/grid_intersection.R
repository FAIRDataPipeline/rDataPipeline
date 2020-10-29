#' Grid intersection
#'
#' @param shapefile shapefile
#' @param gridsize gridsize
#'
#' @export
#' @keywords internal
#'
grid_intersection <- function(shapefile,
                              gridsize) {
 # Check units of shapefile: assuming metres below
  sh_unit <- sf::st_crs(shapefile, parameters = TRUE)$units_gdal
  assertthat::assert_that(sh_unit == "metre",
                          msg = "Unexpected CRS: shapefile distances should be in metres")
     # Generate grid over bounding box of datazone shapefile
    n <- gridsize*1000  # Assume gridsize given in km
    grids <- sf::st_make_grid(sf::st_as_sfc(sf::st_bbox(shapefile)),
                              cellsize = c(n, n))

    width <- sf::st_bbox(shapefile)$xmax - sf::st_bbox(shapefile)$xmin
    height <- sf::st_bbox(shapefile)$ymax - sf::st_bbox(shapefile)$ymin
    num_columns <- ceiling(width / n)
    num_rows <- ceiling(height / n)
    grid_labels <- paste0(1:num_columns, "-", rep(1:num_rows, each = num_columns))
    grid_matrix <- strsplit(grid_labels, "-") %>% lapply(as.numeric)
    grid_matrix <- do.call(rbind.data.frame, grid_matrix)
    colnames(grid_matrix) <- c("x", "y")

    grids <- sf::st_sf(grids, grid_id = grid_labels)

    assertthat::assert_that(max(grid_matrix$x) == num_columns)
    assertthat::assert_that(max(grid_matrix$y) == num_rows)
   subdivisions <- sf::st_intersection(grids, shapefile)

  # Remove cells which intersect simply because they touch the shapefile
  # - These are reduced to lines or points in the intersection
  is_polygon <- sapply(subdivisions$grids, function(x)
    any(grepl("LINE|POINT", class(x))))
  if(length(is_polygon)>0){
    subdivisions <- subdivisions[!is_polygon, , drop = FALSE]
  }
  #Check that datazone components add up to same area as original datazone
  subdivision_area = data.frame(AREAcode = subdivisions$AREAcode,
                                subd_area = st_area(subdivisions))
  subdivision_area = subdivision_area %>%
    group_by(.data$AREAcode) %>%
    summarise(subd_area=sum(.data$subd_area))
  datazone_area = data.frame(AREAcode = shapefile$AREAcode,
                             dz_area = st_area(shapefile))
  subdivision_area = subdivision_area %>%
    left_join(datazone_area, by = "AREAcode")
  subdivision_area$difference = subdivision_area$subd_area - subdivision_area$dz_area
  assertthat::assert_that(all(round(as.numeric(subdivision_area$difference)) == 0))

  # Adjust grid labels to match
  ind <- which(grid_labels %in% unique(subdivisions$grid_id))
  grid_matrix <- grid_matrix[ind,]

  tmp <- sort(apply(grid_matrix, 1, function(x) paste(x[1], x[2], sep = "-")))
  assertthat::assert_that(all(tmp == sort(unique(subdivisions$grid_id))))

  # Use grid to subdivide shapefile
  list(subdivisions = subdivisions,
       grid_matrix = grid_matrix)
}
