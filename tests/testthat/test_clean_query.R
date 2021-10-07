context("Testing clean_query()")

endpoint <- Sys.getenv("FDP_endpoint")

query_1 <- list(storage_location = paste0(endpoint, "storage_location/2"))
query_2 <- list(storage_location = "2")
query_3 <- "string"
query_4 <- 5
query_5 <- NULL
query_6 <- list(storage_root = paste0(endpoint, "text_file/"))
query_7 <- list(release_date = Sys.time())

test_that("clean_query produces correct results", {
  expect_equal(clean_query(data = query_1, endpoint = endpoint), query_2)
  expect_equal(clean_query(data = query_2, endpoint = endpoint), query_2)
  expect_equal(clean_query(data = query_3, endpoint = endpoint),
               as.list(query_3))
  expect_equal(clean_query(data = query_4, endpoint = endpoint),
               as.list(query_4))
  expect_equal(clean_query(data = query_5, endpoint = endpoint), as.list(NULL))
  expect_equal(clean_query(data = query_6, endpoint = endpoint), query_6)
  expect_equal(clean_query(data = query_7, endpoint = endpoint), query_7)
})
