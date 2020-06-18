context("Testing create_number()")

test_that("function behaves as it should", {

  # Incorrect file name should throw error
  testthat::expect_error(create_number(toml_filename = "test_number",
                                       value = 3.0,
                                       name = "shape"))

  # File should be toml format
  create_number(toml_filename = "test_number.toml",
                value = 3.0,
                name = "shape")
  testthat::expect_true(is.toml.file("test_number.toml"))

  # Section name should be "point-estimate"
  testthat::expect_equal(configr::eval.config.sections("test_number.toml"),
                           "point-estimate")
})


# Remove test file
file.remove("test_number.toml")
