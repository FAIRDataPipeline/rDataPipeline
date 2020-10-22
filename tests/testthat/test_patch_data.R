context("Testing patch_data()")

key <- Sys.getenv("SCRC_API_TOKEN")

test_user <- "22"

test_identifier <- sample(1:1000000, 1, replace=TRUE)

datetime <- format(Sys.time(), "%d%m%y%H%M%S")

UID <- paste0("object ", datetime, test_identifier)

object_id <- post_data("object",
                         list(description = UID),
                         key)

issue_id <- get_entry("issue", list(updated_by = test_user))[[1]]$url

if(is.null(issue_id)){
  issue_id <- post_data("issue",
                         list(description = UID),
                         key)
}

invalid_obj_id <- "https://data.scrc.uk/api/unknown/1"
invalid_issue_id <- "https://data.scrc.uk/api/unknown/1"

issue_object_ids <- get_entity("issue", basename(issue_id))$object_issues

data <- list(object_issues = c(issue_object_ids, object_id))

invalid_data <- list(something = "something else")

test_that("Patch data errors when used on object",{
  expect_error(patch_data(object_id, data, key))
})

# Patch Data only produces a message and not an error
test_that("Patch data errors with invalid data",{
  expect_message(patch_data(issue_id, invalid_data, key))
})

test_that("Patch Data returns a list on success and that fields match",{
  expect_message(SCRCdataAPI:::patch_data(issue_id, data, key))
  issue_obj_ids <- list(object_issues = get_entity("issue", basename(issue_id))$object_issues)
  data$object_issues <- as.list(sort(unlist(data$object_issues), decreasing = TRUE))
  expect_equal(issue_obj_ids, data)
})

