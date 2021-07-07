context("Testing get_tables()")

endpoint <- Sys.getenv("FDP_endpoint")
if (grepl("localhost", endpoint)) run_server()

test_that("get_tables returns a character vector of tables", {
  expect_silent(get_tables())
  expect_true(is.character(get_tables(endpoint)))
  expect_true(is.character(get_tables(endpoint)))
})
