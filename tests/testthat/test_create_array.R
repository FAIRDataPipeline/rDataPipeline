library(hdf5r)

context("Testing create_array()")

filename <- "test_array.h5"
component <- "level"
df <- data.frame(a = 1:2, b = 3:4)

test_that("incorrect file name throws error", {
  testthat::expect_error(
    create_array(filename = "test_array",
                 component = component,
                 array = as.matrix(df),
                 dimension_names = list(rowvalue = rownames(df),
                                        colvalue = colnames(df)))
  )
})

test_that("incorrect array format throws error", {
  testthat::expect_error(
    create_array(filename = filename,
                 component = component,
                 array = df,
                 dimension_names = list(rowvalue = rownames(df),
                                        colvalue = colnames(df)))
  )
})

test_that("incorrect dimension_names format throws error", {
  testthat::expect_error(
    create_array(filename = filename,
                 component = component,
                 array = as.matrix(df),
                 dimension_names = list(rowvalue = data.frame(rownames(df)),
                                        colvalue = colnames(df)))
  )
})

test_that("incorrect dimension_names length throws error", {
  testthat::expect_error(
    create_array(filename = filename,
                 component = component,
                 array = as.matrix(df),
                 dimension_names = list(rowvalue = 1:3,
                                        colvalue = colnames(df)))
  )

  testthat::expect_error(
    create_array(filename = filename,
                 component = component,
                 array = as.matrix(df),
                 dimension_names = list(rowvalue = rownames(df),
                                        colvalue = 1))
  )
})

test_that(".h5 file is generated", {
  create_array(filename = filename,
               component = component,
               array = as.matrix(df),
               dimension_names = list(rowvalue = rownames(df),
                                      colvalue = colnames(df)))
  testthat::expect_true(hdf5r::is.h5file(filename))
})

test_that("component name is level", {
  file.h5 <- H5File$new(filename, mode = "r")
  testthat::expect_equal(names(file.h5), component)
  file.h5$close_all()
})


# Remove test file
file.remove(filename)
