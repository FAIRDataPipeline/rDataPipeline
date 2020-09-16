context("Testing create_table() on a basic table")

filename <- "test_table.h5"
filename_2 <- "test_table2.h5"
filename_3 <- "test_table3.h5"
filepath <- paste0("data-raw/", filename_2)
df <- data.frame(a = 1:2, b = 3:4)
component <- "level"
component_2 <- "level/level_2"

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
  file.h5 <- rhdf5::H5Fopen(filename)
  tmp <- gsub("/", "", h5ls(file.h5)$group)[-1]
  testthat::expect_equal(tmp, component)
  rhdf5::h5closeAll()
})

file.remove(filename)


# -------------------------------------------------------------------------

# context("Testing create_table() on a multi-class table")
#
# filename <- "test_table.h5"
# multiclass_df <- data.frame(a = letters[1:2], b = 3:4)
# component <- "level"
#
# test_that("function behaves as it should with multiple classes", {
#   create_table(filename = filename,
#                component = component,
#                df = multiclass_df)
#
#   tab <- read_table(filename = filename,
#                     path = ".",
#                     component = component)
#
#   testthat::expect_equal(tab, multiclass_df)
# })
#
# file.remove(filename)

test_that("directory is created when supplying path",{
    create_table(filename = filename_2,
                 path = "data-raw",
                 component = component_2,
                 df = df)
    testthat::expect_true(hdf5r::is.h5file(filepath))
})

file.remove(filepath)

test_that("create tables works with rownames and units",{
  create_table(filename = filename_3,
               row_names = c("row_1", "row_2"),
               column_units = c("days", "months"),
               component = component,
               df = df)
  testthat::expect_true(hdf5r::is.h5file(filename_3))
})

file.remove(filename_3)








