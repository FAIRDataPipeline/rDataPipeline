context("Test new_text_file")

# get the token
key <- Sys.getenv("SCRC_API_TOKEN")

sleep_time <- 0.5

test_user <- "22"

test_identifier <- sample(1:1000000, 1, replace=TRUE)

UID <- paste0("text_file ", format(Sys.time(), "%d%m%y%H%M%S"), test_identifier)

test_that("new_text_file creates a new text file", {
  expect_true(is.character(new_text_file(UID,
                                         key)))
})

## new_text_file does not show a message if the text file already exists
# test_that("new_text_file produces a message if text file already exists", {
#   expect_message(expect_true(is.character(new_text_file(UID,
#                                          key))))
# })
