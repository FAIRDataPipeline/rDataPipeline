context("Test new_source")

# get the token
key <- Sys.getenv("SCRC_API_TOKEN")

sleep_time <- 0.5

test_user <- "22"

test_identifier <- sample(1:100, 1, replace=TRUE)

datetime <- format(Sys.time(), "%d%m%y%H%M%S")

name <- paste0("source test ", datetime, test_identifier)
abbreviation <- paste0("source ", datetime, test_identifier)
name_1 <- paste0(name, "1")
abbreviation_1 <- paste0("source_", datetime, test_identifier, "1")
name_ <- paste0("sourcetest", datetime, test_identifier)
website <- paste0("https://", name_, ".com")


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

