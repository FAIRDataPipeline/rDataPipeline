context("Testing new_namespace()")

name <- paste0("test_new_namespace_",
               openssl::sha1(x = as.character(Sys.time())))

run_server()

test_that("new entry in namespace returns API URL", {
  expect_true(grepl("namespace", new_namespace(name = name)))
})

test_that("existing entry in namespace returns API URL", {
  expect_true(grepl("namespace", new_namespace(name)))
})

stop_server()
