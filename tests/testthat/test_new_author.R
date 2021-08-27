context("Testing new_author()")

name <- paste0("test_new_author_",
                      openssl::sha1(x = as.character(Sys.time())))

endpoint <- Sys.getenv("FDP_endpoint")

test_that("new entry in author returns an API URL", {
  expect_true(grepl("author", new_author(name = name,
                                         # identifier = ,
                                         endpoint = endpoint)))
})

test_that("existing entry in author returns an API URL", {
  expect_true(grepl("author", new_author(name = name,
                                         # identifier = ,
                                         endpoint = endpoint)))
})
