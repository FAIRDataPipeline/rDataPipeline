context("Test new_code_repo_release")

# get the token
key <- Sys.getenv("SCRC_API_TOKEN")

sleep_time <- 0.5

test_user <- "22"

name <- paste0("Code_Repo_Release_Test_", format(Sys.time(), "%d%m%y%H%M%S"))
version <- create_version_number()
website <- paste0("https://www.github.com/ScottishCovidResponce/", name)

object_id <- post_data("object",
                         list(description = name),
                         key)
test_that("New code repo release returns a character",{
  expect_true(is.character(new_code_repo_release(name, version, website, object_id, key)))
})

# test_that("code_repo_release produces a message if the code_repo_release already exists", {
#   expect_message(expect_true(is.character(new_code_repo_release(name, version, website, object_id, key))))
# })
