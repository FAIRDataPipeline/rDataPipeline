context("Testing new_keyword()")

keyphrase <- paste0("test_new_keyword_",
                    openssl::sha1(x = as.character(Sys.time())))

run_server()

object_url <- post_data("object",
                        list(description = paste0(keyphrase, " Test")))

test_that("new entry in keyword returns API URL", {
  expect_true(grepl("keyword", new_keyword(object_url = object_url,
                                           keyphrase = keyphrase)))
})

test_that("existing entry in keyword returns API URL", {
  expect_true(grepl("keyword", new_keyword(object_url = object_url,
                                           keyphrase = keyphrase)))
})

stop_server()
