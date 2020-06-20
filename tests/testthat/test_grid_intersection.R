
test_that("grid_intersection() should fail if shapefile not in metres", {
  # Grid intersection takes a gridsize in km and assumes the shapefile uses a CRS projected in metres -
  # we should not be silently producing an invalid result when the CRS uses a different unit
  triangle <- sf::st_sfc(sf::st_polygon(list(rbind(c(0,0), c(10,0), c(10,10), c(0,0)))))
  shapefile <- sf::st_sf(triangle, crs = 3418)  # shapefile in NAD83, which uses US_survey_foot as the unit

  testthat::expect_error(grid_intersection(shapefile, 1), regexp = "Unexpected CRS")
})
