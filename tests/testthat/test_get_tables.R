context("Testing get_tables()")

endpoint <- Sys.getenv("FDP_endpoint")

test_that("get_tables returns a character vector of tables", {
  expect_silent(get_tables())
  expect_true(is.character(get_tables(endpoint)))
  expect_true(is.character(get_tables(endpoint)))
})
