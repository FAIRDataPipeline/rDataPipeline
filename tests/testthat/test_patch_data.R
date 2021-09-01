# context("Testing patch_data()")
#
# test_user <- "22"
# test_identifier <- sample(1:1000000, 1, replace=TRUE)
# datetime <- format(Sys.time(), "%d%m%y%H%M%S")
# UID <- paste0("object ", datetime, test_identifier)
#
# object_url <- post_data("object", list(description = UID))
# issue_url <- post_data("issue", list(description = UID))
# invalid_obj_url <- "https://data.scrc.uk/api/unknown/1"
# invalid_issue_url <- "https://data.scrc.uk/api/unknown/1"
# issue_object_urls <- get_entity(issue_url)$object_issues
#
# data <- list(object_issues = c(issue_object_urls, object_url))
# invalid_data <- list(something = "something else")
#
# test_that("Patch data errors when used on object",{
#   expect_error(patch_data(object_url, data))
# })
#
# # Patch Data only produces a message and not an error
# test_that("Patch data errors with invalid data",{
#   expect_message(patch_data(issue_url, invalid_data))
# })
#
# test_that("Patch Data returns a list on success and that fields match",{
#   expect_message(patch_data(issue_url, data))
#   issue_obj_ids <- list(object_issues = get_entity(issue_url)$object_issues)
#   data$object_issues <- as.list(sort(unlist(data$object_issues), decreasing = TRUE))
#   expect_equal(issue_obj_ids, data)
# })
