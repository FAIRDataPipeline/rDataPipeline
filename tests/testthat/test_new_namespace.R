context("Testing new_namespace()")

name <- paste0("test_new_namespace_",
               openssl::sha1(x = as.character(Sys.time())))

endpoint <- Sys.getenv("FDP_endpoint")

test_that("new entry in namespace returns API URL", {
  expect_true(grepl("namespace", new_namespace(name = name,
                                               full_name = name,
                                               # website = ,
                                               endpoint = endpoint)))
})

test_that("existing entry in namespace returns API URL", {
  expect_true(grepl("namespace", new_namespace(name = name,
                                               full_name = name,
                                               # website = ,
                                               endpoint = endpoint)))
})
