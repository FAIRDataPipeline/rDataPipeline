context("Testing get_tables()")

endpoint <- "https://data.scrc.uk/api/"

run_server()

test_that("get_tables returns a character vector of tables", {
  expect_silent(get_tables())
  expect_true(is.character(get_tables(endpoint)))
  expect_true(is.character(get_tables(endpoint)))
})
