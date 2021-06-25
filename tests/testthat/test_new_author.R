context("Testing new_author()")

family_name <- paste0("test_new_author_",
                      openssl::sha1(x = as.character(Sys.time())))
personal_name <- paste0("test_new_author_",
                        openssl::sha1(x = as.character(Sys.time())))

run_server()

test_that("new entry in author returns an API URL", {
  expect_true(grepl("author", new_author(family_name, personal_name)))
})

test_that("existing entry in author returns an API URL", {
  expect_true(grepl("author", new_author(family_name, personal_name)))
})

stop_server()
