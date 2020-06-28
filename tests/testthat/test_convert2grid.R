library(dplyr)

# Basic map used in all tests: a square which divided into a 4x4 grid (where each cell then represents an area)
square <- sf::st_sfc(sf::st_polygon(list(rbind(c(0,0), c(1000,0), c(1000,1000), c(0,1000), c(0,0)))))
outer_shapefile <- sf::st_sf(square, crs = 27700)

basic_shapefile <- grid_intersection(outer_shapefile, gridsize = 1/4)$subdivisions %>%
  mutate(AREAcode = LETTERS[1:16]) %>%
  select(-grid_id)


## Basic functionality
#   - A grid to sumarise to - simply divides the map into fewer squares:
#     - Here, each grid cell encompasses 4 map full map areas (i.e. no areas are split between grid cells)
subdivisions <- grid_intersection(basic_shapefile, gridsize = 1/2)$subdivisions


#   - Test with a single data column
test_that("convert2grid() accurately divides population into grid", {
  pop_data <- data.frame(AREAcode = LETTERS[1:16],
                         Population = sample(1e6, size = 16))

  result <- convert2grid(dat = pop_data, shapefile = basic_shapefile, subdivisions = subdivisions)

  expected_counts <- sf::st_drop_geometry(subdivisions) %>%
    left_join(pop_data, by = "AREAcode") %>%
    group_by(grid_id) %>%
    summarise(Population = sum(Population))

  testthat::expect_equal(as.numeric(result$grid_pop), expected_counts$Population)
})


#   - Test with a multiple data columns (age classes)
test_that("convert2grid() accurately divides age classes into grid", {
  pop_data <- data.frame(AREAcode = LETTERS[1:16],
                         Age1 = sample(1e6, size = 16),
                         Age2 = sample(1e6, size = 16),
                         Age3 = sample(1e6, size = 16),
                         Age4 = sample(1e6, size = 16))

  result <- convert2grid(dat = pop_data, shapefile = basic_shapefile, subdivisions = subdivisions)

  expected_counts <- sf::st_drop_geometry(subdivisions) %>%
    left_join(pop_data, by = "AREAcode") %>%
    group_by(grid_id) %>%
    summarise_at(vars(starts_with("Age")), ~ sum(.)) %>%
    select(-grid_id) %>%
    as.matrix()

  testthat::expect_equal(result$grid_pop, expected_counts)
})


## Integer handling
# - Since we are dividing a population, only integer counts are valid
# - This requires special handling at edges, where a shapefile area is intersected by the grid

# Create a 5x5 map and a 2x2 grid:
#  - Each grid cell then contains 4 full areas, 4 half areas, and 1 quarter of
#    an area (plot subdivisions object to visualise this)
shapefile <- grid_intersection(outer_shapefile, gridsize = 1/5)$subdivisions %>%
  mutate(AREAcode = LETTERS[1:25]) %>%
  select(-grid_id)

subdivisions <- grid_intersection(shapefile, gridsize = 1/2)$subdivisions


test_that("convert2grid() divides population in areas split between grid cells by area", {
  pop_size <- 1e6  # Will always yield integers, making it easier to anticipate result
  pop_data <- data.frame(AREAcode = LETTERS[1:25],
                         Population = pop_size)

  result <- convert2grid(dat = pop_data, shapefile = shapefile, subdivisions = subdivisions)
  expected_result <- (pop_size * 4) + (pop_size * 4 * 0.5) + (pop_size * 0.25)

  testthat::expect_equal(as.numeric(result$grid_pop), rep(expected_result, 4))
})


test_that("convert2grid() returns only integers", {
  pop_size <- c(3, 5, 6, 7, 9, 10, 11, 13, 14)  # Not divisible by 4, so would yield non-integer numbers unless this is handled by convert2grid
  pop_data <- data.frame(AREAcode = LETTERS[1:25],
                         Population = 3)#sample(pop_size, size = 25, replace = TRUE))

  result <- convert2grid(dat = pop_data, shapefile = shapefile, subdivisions = subdivisions)
  testthat::expect_true(all(result$grid_pop %% 1 == 0))
})


test_that("convert2grid() maintains population size when correcting for non-integer results", {
  pop_size <- c(3, 5, 6, 7, 9, 10, 11, 13, 14)  # Not divisible by 4, so would yield non-integer numbers unless this is handled by convert2grid
  pop_data <- data.frame(AREAcode = LETTERS[1:25],
                         Population = sample(pop_size, size = 25, replace = TRUE))

  result <- convert2grid(dat = pop_data, shapefile = shapefile, subdivisions = subdivisions)
  testthat::expect_equal(sum(result$grid_pop), sum(pop_data$Population))
})
