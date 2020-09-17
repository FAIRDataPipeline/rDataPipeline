context("Testing create_distribution()")

dist <- list(name = "latency",
             distribution = "gamma",
             parameters = list(shape = 2.0, scale = 3.0))

test_that("incorrect filename throws an error", {
  testthat::expect_error(
    create_distribution(filename = "test_distribution.tom",
                        path = "data-raw",
                        distribution = dist)
  )
})

test_that("output is a toml file", {
  create_distribution(filename = "test_distribution.toml",
                      path = "data-raw",
                      distribution = dist)
  testthat::expect_true(is.toml.file("data-raw/test_distribution.toml"))
})

test_that("function works with missing path", {
  create_distribution(filename = "test_distribution_1.toml",
                      distribution = dist)
  testthat::expect_true(is.toml.file("test_distribution_1.toml"))
})

# Remove test file
file.remove("data-raw/test_distribution.toml")
file.remove("test_distribution_1.toml")
