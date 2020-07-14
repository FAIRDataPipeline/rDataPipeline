context("Testing create_number()")

test_that("function behaves as it should", {

  # Incorrect file name should throw error
  testthat::expect_error(create_number(filename = "test_number",
                                       path = "data-raw",
                                       value = 3.0,
                                       name = "shape"))

  # File should be toml format
  create_number(filename = "test_number.toml",
                path = "data-raw",
                value = 3.0,
                name = "shape")
  testthat::expect_true(is.toml.file("data-raw/test_number.toml"))
})


# Remove test file
file.remove("test_number.toml")
