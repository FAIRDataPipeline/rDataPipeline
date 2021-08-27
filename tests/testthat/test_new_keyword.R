context("Testing new_keyword()")

keyphrase <- paste0("test_new_keyword_",
                    openssl::sha1(x = as.character(Sys.time())))

endpoint <- Sys.getenv("FDP_endpoint")

object_url <- post_data("object",
                        list(description = paste0(keyphrase, " Test")),
                        endpoint = endpoint)

test_that("new entry in keyword returns API URL", {
  expect_true(grepl("keyword", new_keyword(object_url = object_url,
                                           keyphrase = keyphrase,
                                           endpoint = endpoint)))
})

test_that("existing entry in keyword returns API URL", {
  expect_true(grepl("keyword", new_keyword(object_url = object_url,
                                           keyphrase = keyphrase,
                                           endpoint = endpoint)))
})
