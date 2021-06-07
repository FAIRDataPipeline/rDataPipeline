context("Testing new_namespace()")

sleep_time <- 0.5

test_user <- "22"

test_identifier <- sample(1:1000000, 1, replace=TRUE)

name <- paste0("namespace ", format(Sys.time(), "%d%m%y%H%M%S"), test_identifier)

run_server()

test_that("new_namespace posts to registry", {
  expect_true(is.character(new_namespace(name)))
})

test_that("new_namespace returns URI if the namespace already exists", {
  expect_true(is.character(new_namespace(name)))
})

stop_server()
