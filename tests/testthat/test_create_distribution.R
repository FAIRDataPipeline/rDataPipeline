context("Testing create_distribution()")

test_that("function behaves as it should", {
  # Incorrect file name should throw error
  testthat::expect_error(
    create_distribution(filename = "test_distribution.tom",
                        path = "data-raw",
                        name = "latency",
                        distribution = "gamma",
                        parameters = list(shape = 2.0, scale = 3.0))
  )

  # File should be toml format
  create_distribution(filename = "test_distribution.toml",
                      path = "data-raw",
                      name = "latency",
                      distribution = "gamma",
                      parameters = list(shape = 2.0, scale = 3.0))
  testthat::expect_true(is.toml.file("data-raw/test_distribution.toml"))
})


# Remove test file
file.remove("test_distribution.toml")
