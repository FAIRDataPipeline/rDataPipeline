library(hdf5r)
library(SCRCdataAPI)

context("Testing create_table()")

filename <- "test_table.h5"
component <- "level"

test_that("incorrect file name throws an error", {
  testthat::expect_error(create_table(filename = "test_table",
                                      component = component,
                                      df = data.frame(a = 1:2, b = 3:4)))
})

test_that("incorrect df format throws an error", {
  testthat::expect_error(
    create_table(filename = filename,
                 component = component,
                 df = as.matrix(data.frame(a = 1:2, b = 3:4)))
  )
})

test_that(".h5 file is generated", {
  create_table(filename = filename,
               component = component,
               df = data.frame(a = 1:2, b = 3:4))
  testthat::expect_true(hdf5r::is.h5file(filename))
})

test_that("component name is level", {
  file.h5 <- H5File$new(filename, mode = "r")
  testthat::expect_equal(names(file.h5), component)
  file.h5$close_all()
})

file.remove(filename)


test_that("function behaves as it should", {
  create_table(filename = filename,
               component = component,
               df = data.frame(a = letters[1:2], b = 3:4))

  tab <- read_table(filename = filename,
                    path = ".",
                    component = component)

  testthat::expect_equal(tab, data.frame(a = letters[1:2], b = 3:4))
})

file.remove(filename)









