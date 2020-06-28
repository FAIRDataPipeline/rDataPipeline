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


test_that("convert2grid() distributes integers broadly equally when there are ties", {
  # When several cells cover the same proportion of the map and the underlying areas have the same population size,
  # they should receive the same number of individuals - this may not be possible while maintaining integers, but
  # at least no cell should be favoured over another when dividing the few remaining individuals among cells
  pop_size <- 3
  pop_data <- data.frame(AREAcode = LETTERS[1:25],
                         Population1 = 3)  # This means each grid cell should actually have the same number
  expected_result <- (pop_size * 4) + (pop_size * 4 * 0.5) + (pop_size * 0.25) %>%
    round()

  result <- convert2grid(dat = pop_data, shapefile = shapefile, subdivisions = subdivisions)

  # Expect a tolerance of +/- 1
  testthat::expect_equal(range(result$grid_pop), c(expected_result - 1, expected_result + 1))
})


test_that("convert2grid() distributes integers to the correct cells", {
  # When correcting for non-integer cell counts, the cells closest to an integer should be adjusted
  # - Create a shapefile which is larger than the geometry it contains
  # - This means the grid will extend beyond the edge of the geom
  max_dim <- 1000
  tr_offset <- max_dim + 5
  map_extent <- max_dim + tr_offset
  square_coords <- rbind(c(0,0), c(max_dim,0), c(max_dim,max_dim), c(0,max_dim), c(0,0))
  triangle_coords <- rbind(c(tr_offset, tr_offset),
                           c(max_dim + tr_offset, tr_offset),
                           c(max_dim + tr_offset, max_dim + tr_offset),
                           c(tr_offset, tr_offset))
  polygon <- sf::st_sfc(sf::st_polygon(list(square_coords)),
                        sf::st_point(c(tr_offset, tr_offset)))
  complex_shapefile <- sf::st_sf(polygon, crs = 27700) %>%
    mutate(AREAcode = LETTERS[1:2])

  subdivisions <- grid_intersection(complex_shapefile, gridsize = map_extent/5/1000)$subdivisions

  # Map contains a single cell, while the grid contains 4 complete and 5 partial cells
  # - Expect the 4 complete cells to receive 2, 4 largest partial cells to reveive 1, and the remaining,
  #   smallest, cell to receive 0:
  pop_data <- data.frame(AREAcode = LETTERS[1:2],
                         Population1 = c(2*4 + 1*4, 0))

  result <- convert2grid(dat = pop_data, shapefile = complex_shapefile, subdivisions = subdivisions)

  cell_size_order <- sf::st_area(subdivisions) %>%
    order(decreasing = TRUE)

  size_ordered_result <- result$grid_pop[cell_size_order, 1]

  testthat::expect_equal(size_ordered_result, c(2,2,2,2, 1,1,1,1, 0))
})
