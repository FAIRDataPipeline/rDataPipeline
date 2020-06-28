DEFAULT_CRS <- 27700  # EPSG code for the Ordnance Survey National Grid


# Check inputs:
test_that("grid_intersection() should fail if shapefile not in metres", {
  # Grid intersection takes a gridsize in km and assumes the shapefile uses a CRS projected in metres -
  # we should not be silently producing an invalid result when the CRS uses a different unit
  triangle <- sf::st_sfc(sf::st_polygon(list(rbind(c(0,0), c(10,0), c(10,10), c(0,0)))))
  shapefile <- sf::st_sf(triangle, crs = 3418)  # shapefile in NAD83, which uses US_survey_foot as the unit

  testthat::expect_error(grid_intersection(shapefile, 1), regexp = "Unexpected CRS")
})


# Check results
test_that("grid_intersection() should produce expected number of cells", {
  max_dim <- 1000
  square <- sf::st_sfc(sf::st_polygon(list(rbind(c(0,0), c(max_dim,0), c(max_dim,max_dim), c(0,max_dim), c(0,0)))))
  shapefile <- sf::st_sf(square, crs = DEFAULT_CRS)

  result <- grid_intersection(shapefile, max_dim/4/1000)  # 4 x 4 grid

  testthat::expect_equal(length(result$subdivisions$grids), 4*4)
  testthat::expect_equal(nrow(result$grid_matrix), 4*4)
})


test_that("grid_intersection() should produce cells of expected size", {
  max_dim <- 1000
  square <- sf::st_sfc(sf::st_polygon(list(rbind(c(0,0), c(max_dim,0), c(max_dim,max_dim), c(0,max_dim), c(0,0)))))
  shapefile <- sf::st_sf(square, crs = DEFAULT_CRS)

  result <- grid_intersection(shapefile, max_dim/4/1000)
  cell_size <- sf::st_bbox(result$subdivisions$grids[[1]])

  testthat::expect_equal(cell_size[["xmin"]], 0)
  testthat::expect_equal(cell_size[["ymin"]], 0)
  testthat::expect_equal(cell_size[["xmax"]], max_dim/4)
  testthat::expect_equal(cell_size[["ymax"]], max_dim/4)
})


# Returned grid should be clipped to the extent of the input shapefile / features (i.e. return an actual intersection):
# - Here we need a more complex shapefile
max_dim <- 1000
tr_offset <- max_dim*2
map_extent <- max_dim + tr_offset
square_coords <- rbind(c(0,0), c(max_dim,0), c(max_dim,max_dim), c(0,max_dim), c(0,0))
triangle_coords <- rbind(c(tr_offset, tr_offset),
                         c(max_dim + tr_offset, tr_offset),
                         c(max_dim + tr_offset, max_dim + tr_offset),
                         c(tr_offset, tr_offset))
polygon <- sf::st_sfc(sf::st_polygon(list(square_coords)),
                      sf::st_polygon(list(triangle_coords)))
complex_shapefile <- sf::st_sf(polygon, crs = DEFAULT_CRS)


test_that("grid_intersection() result should not extend beyond shapefile bounding box", {
  bounding_box <- sf::st_bbox(complex_shapefile)
  result <- grid_intersection(complex_shapefile, bounding_box[["xmax"]]/12/1000)

  testthat::expect_equal(sf::st_bbox(result$subdivisions), bounding_box)
})


test_that("grid_intersection() should return only parts of grid which intersect with features", {
  grid_size <- map_extent/12/1000
  result <- grid_intersection(complex_shapefile, grid_size)

  # Define the empty area of complex_shapefile by creating a larger polygon with the original shapes as holes:
  covering_square_coords <- rbind(c(0, 0), c(map_extent, 0), c(map_extent, map_extent), c(0, map_extent), c(0, 0))
  empty_area <- sf::st_sfc(sf::st_polygon(list(covering_square_coords, square_coords, triangle_coords)))
  empty_area <- sf::st_sf(empty_area, crs = DEFAULT_CRS)

  testthat::expect_false(all(sf::st_covers(result$subdivisions, empty_area, sparse = FALSE)))
})


test_that("grid_intersection() should produce cells of expected size at edges of irregular features", {
  max_dim <- 1000
  grid_size <- max_dim/4

  triangle <- sf::st_sfc(sf::st_polygon(list(rbind(c(0,0), c(max_dim,0), c(max_dim,max_dim), c(0,0)))))
  tr_shapefile <- sf::st_sf(triangle, crs = DEFAULT_CRS)

  result <- grid_intersection(tr_shapefile, grid_size/1000)

  # Expected area of cell 1-1 (which should be a triangle since it's at the left corner of the map)
  expected_area <- grid_size*grid_size/2
  expected_area <- units::set_units(expected_area, "m^2")

  testthat::expect_equal(sf::st_area(result$subdivisions$grids[1]), expected_area)
})


test_that("grid_intersection() should not return or enumerate non-grid objects", {
  # Expect all features to represent parts of the grid, otherwise, indexing, etc. may be off
  result <- SCRCdataAPI:::grid_intersection(complex_shapefile, gridsize = map_extent/12/1000)

  # All shapefile objects should be of class POLYGON:
  is_polygon <- sapply(result$subdivisions$grids, function(x) "POLYGON" %in% class(x))
  testthat::expect_true(all(is_polygon))
})


test_that("ids returned by grid_intersection() should match in both subdivision and grid_matrix", {
  result <- SCRCdataAPI:::grid_intersection(complex_shapefile, gridsize = map_extent/12/1000)

  matrix_ids <- paste(result$grid_matrix[, 1], result$grid_matrix[, 2], sep = "-")
  testthat::expect_true(all(matrix_ids %in% result$subdivisions$grid_id))
})
