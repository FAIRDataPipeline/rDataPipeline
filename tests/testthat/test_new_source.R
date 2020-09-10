context("Test new_source")

# get the token
key <- Sys.getenv("SCRC_API_TOKEN")

sleep_time <- 0.5

test_user <- "22"

name <- paste0("source_Test_OBJECT_", format(Sys.time(), "%d%m%y%H%M%S"))
abbreviation <- paste0("source_", format(Sys.time(), "%d%m%y%H%M%S"))
name_1 <- paste0(name, "_1")
abbreviation_1 <- paste0("source_", format(Sys.time(), "%d%m%y%H%M%S"))
website <- "https://test.com"


## Website must be a valid url
test_that("new_source creates new source", {
  expect_true(is.character(new_source(name,
                                      abbreviation,
                                      "",
                                      key)))
})

test_that("new_source creates new source with website", {
  expect_true(is.character(new_source(name_1,
                                      abbreviation_1,
                                      website,
                                      key)))
})

test_that("new_source produces a message if the source exists", {
  expect_message(expect_true(is.character(new_source(name,
                                      abbreviation,
                                      "",
                                      key))))
})

