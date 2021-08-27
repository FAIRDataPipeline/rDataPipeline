context("Testing check_exists()")

endpoint <- Sys.getenv("FDP_endpoint")

test_that("invalid table throws an error", {
  testthat::expect_error(check_exists(table = "unknown",
                                      query = ""),
                         regexp = "Table does not exist")
})

test_that("invalid query throws an error", {
  testthat::expect_error(check_exists(table = "object",
                                      query = "unknown=unknown"))
  testthat::expect_error(check_exists(table = "object",
                                      query = "unknown"))
})

test_that("object does not exists returns false", {
  testthat::expect_false(check_exists(table = "object",
                                      query = "description=unknown00001"))
})

description <- paste0("test_check_exists_",
                      openssl::sha1(x = as.character(Sys.time())))

new_object(description = description)

test_that("object exists returns true", {
  testthat::expect_true(check_exists(table = "object",
                                     query = list(description = description)))
})
