library(hdf5r)

context("Testing create_array()")

h5filename <- "test_array.h5"
component <- "level"
df <- data.frame(a = 1:2, b = 3:4)

test_that("function behaves as it should", {
  # Incorrect file name should throw error
  testthat::expect_error(
    create_array(h5filename = "test_array",
                 component = component,
                 array = as.matrix(df),
                 dimension_names = list(rowvalue = rownames(df),
                                        colvalue = colnames(df)))
  )

  # Incorrect array format should throw error
  testthat::expect_error(
    create_array(h5filename = h5filename,
                 component = component,
                 array = df,
                 dimension_names = list(rowvalue = rownames(df),
                                        colvalue = colnames(df)))
  )

  # Incorrect dimension_names format should throw error
  testthat::expect_error(
    create_array(h5filename = h5filename,
                 component = component,
                 array = as.matrix(df),
                 dimension_names = list(rowvalue = data.frame(rownames(df)),
                                        colvalue = colnames(df)))
  )

  # Incorrect dimension_names length should throw error
  testthat::expect_error(
    create_array(h5filename = h5filename,
                 component = component,
                 array = as.matrix(df),
                 dimension_names = list(rowvalue = 1:3,
                                        colvalue = colnames(df)))
  )

  # Incorrect dimension_names length should throw error
  testthat::expect_error(
    create_array(h5filename = h5filename,
                 component = component,
                 array = as.matrix(df),
                 dimension_names = list(rowvalue = rownames(df),
                                        colvalue = 1))
  )

  # File should be h5 format
  create_array(h5filename = h5filename,
               component = component,
               array = as.matrix(df),
               dimension_names = list(rowvalue = rownames(df),
                                      colvalue = colnames(df)))
  testthat::expect_true(hdf5r::is.h5file(h5filename))

  # Component name should be "level"
  file.h5 <- H5File$new(h5filename, mode = "r")
  testthat::expect_equal(names(file.h5), component)
  file.h5$close_all()
})


# Remove test file
file.remove(h5filename)
