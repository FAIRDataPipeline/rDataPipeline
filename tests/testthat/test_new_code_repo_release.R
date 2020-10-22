context("Testing new_code_repo_release()")

# get the token
key <- Sys.getenv("SCRC_API_TOKEN")

sleep_time <- 0.5

test_user <- "22"

test_identifier <- sample(1:1000000, 1, replace=TRUE)

name <- paste0("Code Repo Release ", format(Sys.time(), "%d%m%y%H%M%S"), test_identifier)
version <- create_version_number()

website_indentifier <- paste(sample(letters, 10, FALSE), collapse ="", sep = "")

website <- paste0("https://www.", website_indentifier, ".com")

object_id <- post_data("object",
                         list(description = name),
                         key)
test_that("New code repo release returns a character",{
  expect_true(is.character(new_code_repo_release(name, version, website, object_id, key)))
})

# test_that("code_repo_release produces a message if the code_repo_release already exists", {
#   expect_message(expect_true(is.character(new_code_repo_release(name, version, website, object_id, key))))
# })
