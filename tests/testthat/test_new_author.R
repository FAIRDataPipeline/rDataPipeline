context("Test new_author")

# get the token
key <- Sys.getenv("SCRC_API_TOKEN")

test_user <- "22"

object_id <- get_entry("object", list(updated_by = test_user))[[1]]$url

test_identifier <- sample(1:1000000, 1, replace=TRUE)

UID <- paste0("Author Test", format(Sys.time(), "%d%m%y%H%M%S"), test_identifier)

family_name <- paste(sample(letters, 7, FALSE), collapse ="", sep = "")
personal_name <- paste(sample(letters, 6, FALSE), collapse ="", sep = "")

if(is.null(object_id)){
  object_id <- post_data("object",
                         list(description = UID),
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
