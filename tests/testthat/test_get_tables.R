context("Testing get_tables()")

sleep_time <- 0.5

run_server()

test_that("get_tables returns a character vector of tables", {
  expect_silent(get_tables())
  Sys.sleep(sleep_time)
  expect_true(is.character(get_tables()))
  Sys.sleep(sleep_time)
  expect_true(is.character(get_tables(TRUE)))
})

stop_server()
