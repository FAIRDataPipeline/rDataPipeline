context("testing check_for_hdf")

data_dir <- "data-raw"
dir.create(data_dir, showWarnings = FALSE)
data_product <- get_entry("data_product", list(name = "records/SARS-CoV-2/scotland/cases_and_management"))
object_id <- basename(data_product[[1]]$object)
h5_file <- get_h5_from_object_id(object_id, data_dir)
component <- "testing_location"

test_that("check_for_hdf works with known h5", {
  skip_if_not(file.exists(h5_file))
  expect_true(check_for_hdf5(h5_file, component))
})

test_that("check_for_hdf returns false when it should", {
  expect_false(check_for_hdf5(h5_file, "unknown"))
  expect_false(check_for_hdf5("Unknown", "unknown"))
})

test_that("check_for_hdf errors if invalid file provided", {
  expect_error(check_for_hdf5(NULL, NULL))
  expect_error(check_for_hdf5(NA, NA))
  expect_error(check_for_hdf5(1, 1))
})

file.remove(h5_file)
unlink(data_dir, recursive = TRUE, force = TRUE)
