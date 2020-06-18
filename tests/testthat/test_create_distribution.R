context("Testing create_distribution()")

test_that("function behaves as it should", {
  # Incorrect file name should throw error
  testthat::expect_error(
    create_distribution(toml_filename = "test_distribution",
                        distribution = "gamma",
                        values = c(3.0, 2.0),
                        names = c("shape", "scale"))
  )

  create_distribution(toml_filename = "test_distribution.toml",
                      distribution = "gamma",
                      values = c(3.0, 2.0),
                      names = c("shape", "scale"))

  # File should be toml format
  testthat::expect_true(is.toml.file("test_distribution.toml"))

  # Section name should be "point-estimate"
  testthat::expect_equal(
    configr::eval.config.sections("test_distribution.toml"),
    "distribution"
  )
})


# Remove test file
file.remove("test_distribution.toml")
