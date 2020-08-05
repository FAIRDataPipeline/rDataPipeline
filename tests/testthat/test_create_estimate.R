context("Testing create_estimate()")

test_that("function behaves as it should", {

  # Incorrect file name should throw error
  testthat::expect_error(
    create_estimate(filename = "test_estimate",
                    path = "data-raw",
                    parameters = list(asymptomatic_period = 192.0)))

  # File should be toml format
  create_estimate(filename = "test_estimate.toml",
                  path = "data-raw",
                  parameters = list(asymptomatic_period = 192.0))
  testthat::expect_true(is.toml.file("data-raw/test_estimate.toml"))
})


# Remove test file
file.remove("data-raw/test_estimate.toml")
unlink("data-raw", recursive = TRUE)
