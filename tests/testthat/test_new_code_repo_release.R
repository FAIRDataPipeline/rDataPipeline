context("Testing new_code_repo_release()")

name <- paste0("test_new_code_repo_release_",
               openssl::sha1(x = as.character(Sys.time())))
version <- create_version_number()
website <- paste0("https://www.", gsub("_", "", name), ".com")

endpoint <- Sys.getenv("FDP_endpoint")

object_url <- post_data("object", list(description = name), endpoint = endpoint)

test_that("new entry in code_repo_release returns API URL", {
  expect_true(grepl("code_repo_release",
                    new_code_repo_release(name = name,
                                          version = version,
                                          object_url = object_url,
                                          website = website,
                                          endpoint = endpoint)))
})

test_that("existing entry in code_repo_release returns API URL", {
  expect_true(grepl("code_repo_release",
                    new_code_repo_release(name = name,
                                          version = version,
                                          object_url = object_url,
                                          website = website,
                                          endpoint = endpoint)))
})
