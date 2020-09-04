context("Test new_author")

# get the token
key <- Sys.getenv("SCRC_API_TOKEN")

sleep_time <- 0.5

test_user <- "22"

object_id <- get_entry("object", list(updated_by = test_user))[[1]]$url

family_name <- paste0("Author_Test_", format(Sys.time(), "%d%m%y%H%M%S"))
personal_name <- paste0("Author_Test_", format(Sys.time(), "%d%m%y%H%M%S"))

if(is.null(object_id)){
  source_id <- post_data("object",
                         list(name = UID, abbreviation = formatted_date),
                         key)
}

test_that("New Author returns a character vector", {
  expect_true(is.character(new_author(family_name, personal_name, object_id, key)))
})


######################################################################
##  New Author Does not check if an author exists prior to posting  ##
######################################################################

# test_that("If an author exists a message is returned", {
#   expect_message(expect_true(is.character(new_author(family_name, personal_name, object_id, key))))
# })
