context("Testing check_exists()")

test_that("empty query returns true", {
  testthat::expect_true(check_exists("object", ""))
})

test_that("invalid table throws an error", {
  testthat::expect_error(check_exists("unknown", ""))
})

test_that("invalid query throws an error", {
  testthat::expect_error(check_exists("object", "unknown=unknown"))
  testthat::expect_error(check_exists("object", "unknown"))
})

# test_that("object exists returns true", {
#   testthat::expect_true(check_exists("data_product", "name=population_parameters"))
# })
#
# test_that("object does not exists returns false", {
#   testthat::expect_false(check_exists("object", "description=unknown00001"))
# })
