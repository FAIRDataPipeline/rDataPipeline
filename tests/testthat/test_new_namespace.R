context("Test new_namespace")

# get the token
key <- Sys.getenv("SCRC_API_TOKEN")

sleep_time <- 0.5

test_user <- "22"

name <- paste0("namespace_Test_OBJECT_", format(Sys.time(), "%d%m%y%H%M%S"))

test_that("new_namespace posts to registry", {
  expect_true(is.character(new_namespace(name,
                                         key)))
})

test_that("new_namespace produces a message when the same name is used", {
  expect_message(is.character(new_namespace(name,
                                            key)))
})
