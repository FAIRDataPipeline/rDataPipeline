library(hdf5r)
library(SCRCdataAPI)

context("Testing create_table() on a basic table")

filename <- "test_table.h5"
df <- data.frame(a = 1:2, b = 3:4)
component <- "level"

test_that("incorrect file name throws an error", {
  testthat::expect_error(create_table(filename = "test_table",
                                      component = component,
                                      df = df))
})

test_that("incorrect df format throws an error", {
  testthat::expect_error(
    create_table(filename = filename,
                 component = component,
                 df = as.matrix(df))
  )
})

test_that(".h5 file is generated", {
  create_table(filename = filename,
               component = component,
               df = df)
  testthat::expect_true(hdf5r::is.h5file(filename))
})

test_that("component name is level", {
  file.h5 <- H5File$new(filename, mode = "r")
  testthat::expect_equal(names(file.h5), component)
  file.h5$close_all()
})

file.remove(filename)


# -------------------------------------------------------------------------

context("Testing create_table() on a multi-class table")

filename <- "test_table.h5"
multiclass_df <- data.frame(a = letters[1:2], b = 3:4)
component <- "level"

test_that("function behaves as it should with multiple classes", {
  create_table(filename = filename,
               component = component,
               df = multiclass_df)

  tab <- read_table(filename = filename,
                    path = ".",
                    component = component)

  testthat::expect_equal(tab, multiclass_df)
})

file.remove(filename)









