context("Testing paper_exists")

paper_id <- "10.1001/jama.2020.12248"

sleep_time <- 0.5

test_that("Paper 10.1001/jama.2020.12248 exists", {
  expect_true(paper_exists("10.1001/jama.2020.12248"))
  Sys.sleep(sleep_time)
  expect_silent(as.numeric(paper_exists("10.1001/jama.2020.12248")))
  Sys.sleep(sleep_time)
})

test_that("unknown paper returns false", {
  expect_false(paper_exists("Unknown Paper"))
  Sys.sleep(sleep_time)
  expect_false(paper_exists(NULL))
  Sys.sleep(sleep_time)
  expect_false(paper_exists(NaN))
  Sys.sleep(sleep_time)
  expect_false(paper_exists(Inf))
})
