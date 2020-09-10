context("Test new_issue")

# get the token
key <- Sys.getenv("SCRC_API_TOKEN")

sleep_time <- 0.5

test_user <- "22"

UID <- paste0("issue_Test_OBJECT_", format(Sys.time(), "%d%m%y%H%M%S"))

object_id <- get_entry("object", list(updated_by = test_user))[[1]]$url

if(is.null(object_id)){
  object_id <- post_data("object",
                         list(name = UID),
                         key)
}

object_component_id <- get_entry("object_component", list(updated_by = test_user))[[1]]$url

if(is.null(object_component_id)){
  object_component_id <- post_data("object_component",
                         list(name = UID,
                              object = object_id),
                         key)
}

test_that("new issue creates a new issue on the data registry and associates it with an object", {
  expect_true(is.character(new_issue(severity = 5,
                                     description = UID,
                                     object_issues = list(object_id),
                                     component_issues = list(),
                                     key)))
})

UID <- paste0(UID, "_1")

test_that("new issue creates a new issue on the data registry and associates it with an object", {
  expect_true(is.character(new_issue(severity = 5,
                                     description = UID,
                                     object_issues = list(object_id),
                                     component_issues = list(object_component_id),
                                     key)))
})

UID <- paste0(UID, "_2")

test_that("new issue creates a new issue on the data registry and associates it with an object", {
  expect_true(is.character(new_issue(severity = 5,
                                     description = UID,
                                     object_issues = list(),
                                     component_issues = list(),
                                     key = key)))
})

# Issue allows multiple issues with the same details

# test_that("new issue errors with same issue", {
#   expect_message(expect_true(is.character(new_issue(severity = 5,
#                                      description = UID,
#                                      object_issues = list(),
#                                      component_issues = list(),
#                                      key = key))))
# })
