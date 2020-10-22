context("Testing clean_query()")

query_1 <- list(storage_location = "https://data.scrc.uk/api/storage_location/2")
query_2 <- list(storage_location = "2")
query_3 <- "string"
query_4 <- 5
query_5 <- NULL
query_6 <- list(storage_location = "https://data.scrc.uk/api/storage_location/2",
                storage_root = "")
query_7 <- list(storage_root = "https://data.scrc.uk/api/text_file/")
query_8 <- list(release_date = Sys.time())

test_that("clean_query produces correct results", {
  expect_equal(clean_query(query_1), query_2)
  expect_equal(clean_query(query_2), query_2)
  expect_equal(clean_query(query_3), as.list(query_3))
  expect_equal(clean_query(query_4), as.list(query_4))
  expect_equal(clean_query(query_5), as.list(NULL))
  expect_equal(clean_query(query_6), list(storage_location = "2",
                                                    storage_root = NULL))
  expect_equal(clean_query(query_7), query_7)
  expect_equal(clean_query(query_8), query_8)
})
