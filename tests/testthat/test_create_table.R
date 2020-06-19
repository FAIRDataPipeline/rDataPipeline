library(hdf5r)

context("Testing create_table()")

h5filename <- "test_table.h5"
component <- "level"

test_that("function behaves as it should", {
  # Incorrect file name should throw error
  testthat::expect_error(create_table(h5filename = "test_table",
                                      component = component,
                                      df = data.frame(a = 1:2, b = 3:4)))

  # Incorrect df format should throw error
  testthat::expect_error(
    create_table(h5filename = h5filename,
                 component = component,
                 df = as.matrix(data.frame(a = 1:2, b = 3:4)))
  )

  # File should be h5 format
  create_table(h5filename = h5filename,
               component = component,
               df = data.frame(a = 1:2, b = 3:4))
  testthat::expect_true(hdf5r::is.h5file(h5filename))

  # Component name should be "level"
  file.h5 <- H5File$new(h5filename, mode = "r")
  testthat::expect_equal(names(file.h5), component)
  file.h5$close_all()
})


# Remove test file
file.remove(h5filename)
