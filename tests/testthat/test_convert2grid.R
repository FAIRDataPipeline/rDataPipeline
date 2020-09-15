library(dplyr)

set.seed(123)

context("Testing convert2grid()")

boydorr <- Sys.getenv("BOYDORR")

# Wrap in test_that to enable skipping of tests
test_that("Test Convert to Grid", {
  skip_if(boydorr == "TRUE")
  # Basic map used in all tests: a square which divided into a 4x4 grid (where
  # each cell then represents an area)
  square <- sf::st_sfc(sf::st_polygon(list(rbind(c(0,0), c(1000,0), c(1000,1000),
                                                 c(0,1000), c(0,0)))))
  outer_shapefile <- sf::st_sf(square, crs = 27700)
  outer_shapefile$AREAcode <- 1

  basic_shapefile <- testthat::expect_warning(
    grid_intersection(outer_shapefile,
                      gridsize = 1/4))$subdivisions %>%
    dplyr::mutate(AREAcode = LETTERS[1:16]) %>%
    dplyr::select(-grid_id)


  ## Basic functionality
  #   - A grid to sumarise to - simply divides the map into fewer squares:
  #   - Here, each grid cell encompasses 4 map full map areas (i.e. no areas
  # are split between grid cells)
  grd_size <- 1/2
  subdivisions <- testthat::expect_warning(
    grid_intersection(basic_shapefile,
                      gridsize = grd_size)$subdivisions)
  # plot(subdivisions)

  ## HAVE TO MAKE conversion table will make separate function for this ASAP
  basic_areas <- data.frame(AREAcode = basic_shapefile$AREAcode,
                            basic_area = sf::st_area(basic_shapefile))

  subdivisions_area <- data.frame(grid_id = subdivisions$grid_id,
                                  AREAcode = subdivisions$AREAcode,
                                  subdivision_area = sf::st_area(subdivisions))

  subdivisions_area <- subdivisions_area %>%
    dplyr::left_join(basic_areas, by = "AREAcode") %>%
    dplyr::mutate(grid_area_proportion = as.numeric(subdivision_area /
                                                      basic_area)) %>%
    dplyr::select(grid_id, AREAcode, grid_area_proportion)

  # - Test with a single data column
  test_that("convert2grid() accurately divides population into grid", {
    pop_data <- data.frame(AREAcode = LETTERS[1:16],
                           Population = sample(1e6, size = 16))

    result <- convert2grid(dat = pop_data,
                           shapefile = basic_shapefile,
                           subdivisions = subdivisions,
                           conversion.table = subdivisions_area,
                           grid_size = "grid")
    expected_counts <- sf::st_drop_geometry(subdivisions) %>%
      dplyr::left_join(pop_data, by = "AREAcode") %>%
      dplyr::group_by(grid_id) %>%
      dplyr::summarise(Population = sum(Population))
    result$grid_pop = result$grid_pop[order(result$grid_id)]
    testthat::expect_equal(as.numeric(result$grid_pop),
                           expected_counts$Population)
  })


  #   - Test with a multiple data columns (age classes)
  test_that("convert2grid() accurately divides age classes into grid", {
    pop_data <- data.frame(AREAcode = LETTERS[1:16],
                           Age1 = sample(1e6, size = 16),
                           Age2 = sample(1e6, size = 16),
                           Age3 = sample(1e6, size = 16),
                           Age4 = sample(1e6, size = 16))

    result <- convert2grid(dat = pop_data,
                           shapefile = basic_shapefile,
                           subdivisions = subdivisions,
                           conversion.table = subdivisions_area,
                           grid_size = "grid")
    result$grid_pop = result$grid_pop[order(result$grid_id),]

    expected_counts <- sf::st_drop_geometry(subdivisions) %>%
      dplyr::left_join(pop_data, by = "AREAcode") %>%
      dplyr::group_by(grid_id) %>%
      dplyr::summarise_at(vars(starts_with("Age")), ~ sum(.)) %>%
      tibble::column_to_rownames("grid_id") %>%
      as.matrix()

    testthat::expect_equal(result$grid_pop, expected_counts)
  })


  ## Integer handling
  # - Since we are dividing a population, only integer counts are valid
  # - This requires special handling at edges, where a shapefile area is
  # intersected by the grid

  # Create a 5x5 map and a 2x2 grid:
  #  - Each grid cell then contains 4 full areas, 4 half areas, and 1 quarter of
  #    an area (plot subdivisions object to visualise this)
  shapefile <- testthat::expect_warning(
    grid_intersection(outer_shapefile,
                      gridsize <- 1/5))$subdivisions %>%
    dplyr::mutate(AREAcode = LETTERS[1:25]) %>%
    dplyr::select(-grid_id)

  subdivisions <- testthat::expect_warning(
    grid_intersection(shapefile, gridsize = 1/2))$subdivisions

  shape_areas <- data.frame(AREAcode = shapefile$AREAcode,
                            shape_area = sf::st_area(shapefile))

  subdivisions_area <- data.frame(grid_id = subdivisions$grid_id,
                                  AREAcode = subdivisions$AREAcode,
                                  subdivision_area = sf::st_area(subdivisions))

  subdivisions_area <- subdivisions_area %>%
    dplyr::left_join(shape_areas, by = "AREAcode") %>%
    dplyr::mutate(grid_area_proportion = as.numeric(subdivision_area / shape_area)) %>%
    dplyr::select(grid_id, AREAcode, grid_area_proportion)

  test_that("convert2grid() divides population in areas split between grid cells by area", {
    # Will always yield integers, making it easier to anticipate result
    pop_size <- 1e6
    pop_data <- data.frame(AREAcode = LETTERS[1:25],
                           Population = pop_size)

    result <- convert2grid(dat = pop_data,
                           shapefile = shapefile,
                           subdivisions = subdivisions,
                           conversion.table = subdivisions_area,
                           grid_size = "grid")
    expected_result <- (pop_size * 4) + (pop_size * 4 * 0.5) + (pop_size * 0.25)
    result$grid_pop = result$grid_pop[order(result$grid_id),]

    testthat::expect_equal(as.numeric(result$grid_pop), rep(expected_result, 4))
  })


  test_that("convert2grid() returns only integers", {
    # Not divisible by 4, so would yield non-integer numbers unless this is
    # handled by convert2grid
    pop_size <- c(3, 5, 6, 7, 9, 10, 11, 13, 14)
    pop_data <- data.frame(AREAcode = LETTERS[1:25],
                           Population = 3)#sample(pop_size, size = 25, replace = TRUE))

    result <- convert2grid(dat = pop_data,
                           shapefile = shapefile,
                           subdivisions = subdivisions,
                           conversion.table = subdivisions_area,
                           grid_size = "grid")
    testthat::expect_true(all(result$grid_pop %% 1 == 0))
  })


  test_that("convert2grid() maintains population size when correcting for non-integer results", {
    # Not divisible by 4, so would yield non-integer numbers unless this is
    # handled by convert2grid
    pop_size <- c(3, 5, 6, 7, 9)
    pop_data <- data.frame(AREAcode = LETTERS[1:25],
                           Population = pop_size)

    result <- convert2grid(dat = pop_data,
                           shapefile = shapefile,
                           subdivisions = subdivisions,
                           conversion.table = subdivisions_area,
                           grid_size = "grid")
    testthat::expect_equal(sum(result$grid_pop), sum(pop_data$Population))
  })


  test_that("convert2grid() distributes integers broadly equally when there are ties", {
    # When several cells cover the same proportion of the map and the underlying
    # areas have the same population size, they should receive the same number of
    # individuals - this may not be possible while maintaining integers, but
    # at least no cell should be favoured over another when dividing the few
    # remaining individuals among cells

    # Every cell in the 5 x 5 shapefile gets divided into 4:
    subdivisions <- testthat::expect_warning(
      grid_intersection(shapefile, gridsize = 1/10))$subdivisions

    shape_areas <- data.frame(AREAcode = shapefile$AREAcode,
                              shape_area = sf::st_area(shapefile))

    subdivisions_area <- data.frame(grid_id = subdivisions$grid_id,
                                    AREAcode = subdivisions$AREAcode,
                                    subdivision_area = sf::st_area(subdivisions))

    subdivisions_area <- subdivisions_area %>%
      dplyr::left_join(shape_areas, by = "AREAcode") %>%
      dplyr::mutate(grid_area_proportion = as.numeric(subdivision_area /
                                                        shape_area)) %>%
      dplyr::select(grid_id, AREAcode, grid_area_proportion)

    # Each cell in the shapefile has the same population size, and this is a
    # prime number, so all grid cells will have a rounding issue:
    pop_data <- data.frame(AREAcode = LETTERS[1:25],
                           Population1 = 89)

    # Tests: repeated since result is stochastic
    tolerance <- vector()
    rank_correlation <- vector()

    for (i in 1:10) {
      result <- convert2grid(dat = pop_data,
                             shapefile = shapefile,
                             subdivisions = subdivisions,
                             conversion.table = subdivisions_area,
                             grid_size = "grid")
      tolerance <- c(tolerance,
                     max(result$grid_pop) - min(result$grid_pop))
      rank_correlation <- c(rank_correlation,
                            abs(cor(result$grid_pop[, ],
                                    seq_len(nrow(result$grid_pop)),
                                    method = 'spearman')))
    }


    # Expect a tolerance of +/- 1 in all runs:
    testthat::expect_true(all(tolerance == 1))

    # Expect little correlation with order of cells:
    rank_correlation <- abs(cor(result$grid_pop[, ],
                                seq_len(nrow(result$grid_pop)),
                                method = 'spearman'))
    testthat::expect_true(all(rank_correlation < 0.15))
  })


  test_that("convert2grid() distributes integers to the correct cells", {
    # When correcting for non-integer cell counts, the cells closest to an
    # integer should be adjusted
    # - Create a shapefile which is larger than the geometry it contains
    # - This means the grid will extend beyond the edge of the geom
    max_dim <- 1000
    tr_offset <- max_dim + 5
    map_extent <- max_dim + tr_offset
    square_coords <- rbind(c(0,0), c(max_dim,0), c(max_dim,max_dim),
                           c(0,max_dim), c(0,0))
    triangle_coords <- rbind(c(tr_offset, tr_offset),
                             c(max_dim + tr_offset, tr_offset),
                             c(max_dim + tr_offset, max_dim + tr_offset),
                             c(tr_offset, tr_offset))
    polygon <- sf::st_sfc(sf::st_polygon(list(square_coords)),
                          sf::st_point(c(tr_offset, tr_offset)))
    complex_shapefile <- sf::st_sf(polygon, crs = 27700) %>%
      mutate(AREAcode = LETTERS[1:2])

    subdivisions <- testthat::expect_warning(
      grid_intersection(complex_shapefile,
                        gridsize = map_extent/5/1000))$subdivisions

    complex_areas <- data.frame(AREAcode = complex_shapefile$AREAcode,
                                complex_area = sf::st_area(complex_shapefile))

    subdivisions_area <- data.frame(grid_id = subdivisions$grid_id,
                                    AREAcode = subdivisions$AREAcode,
                                    subdivision_area = sf::st_area(subdivisions))

    subdivisions_area <- subdivisions_area %>%
      dplyr::left_join(complex_areas, by = "AREAcode") %>%
      dplyr::mutate(grid_area_proportion = as.numeric(subdivision_area /
                                                        complex_area)) %>%
      dplyr::select(grid_id, AREAcode, grid_area_proportion)

    # Map contains a single cell, while the grid contains 4 complete and 5
    # partial cells
    # - Expect the 4 complete cells to receive 2, 4 largest partial cells to
    # reveive 1, and the remaining, smallest, cell to receive 0:
    pop_data <- data.frame(AREAcode = LETTERS[1:2],
                           Population1 = c(2*4 + 1*4, 0))

    result <- convert2grid(dat = pop_data,
                           shapefile = complex_shapefile,
                           subdivisions = subdivisions,
                           conversion.table = subdivisions_area,
                           grid_size = "grid")

    cell_size_order <- sf::st_area(subdivisions) %>%
      order(decreasing = TRUE)

    size_ordered_result <- result$grid_pop[cell_size_order, 1]

    testthat::expect_equal(as.vector(size_ordered_result), c(2,2,2,2, 1,1,1,1, 0))
  })

})

