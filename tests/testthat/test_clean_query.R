context("Testing clean_query()")

query_1 <- list(storage_location = "http://localhost:8000/api/storage_location/2")
query_2 <- list(storage_location = "2")
query_3 <- "string"
query_4 <- 5
query_5 <- NULL
query_6 <- list(storage_root = "http://localhost:8000/api/text_file/")
query_7 <- list(release_date = Sys.time())

test_that("clean_query produces correct results", {
  expect_equal(clean_query(query_1), query_2)
  expect_equal(clean_query(query_2), query_2)
  expect_equal(clean_query(query_3), as.list(query_3))
  expect_equal(clean_query(query_4), as.list(query_4))
  expect_equal(clean_query(query_5), as.list(NULL))
  expect_equal(clean_query(query_6), query_6)
  expect_equal(clean_query(query_7), query_7)
})
