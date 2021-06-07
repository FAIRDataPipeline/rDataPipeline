context("Testing new_issue()")

sleep_time <- 0.5

test_user <- "22"

test_identifier <- sample(1:1000000, 1, replace=TRUE)

UID <- paste0("issue test", format(Sys.time(), "%d%m%y%H%M%S"), test_identifier)

run_server()

object_id <- get_entry("object", list(updated_by = test_user))[[1]]$url

if(is.null(object_id)){
  object_id <- post_data("object",
                         list(description = UID))
}

object_component_id <- get_entry("object_component",
                                 list(updated_by = test_user))[[1]]$url

if(is.null(object_component_id)){
  object_component_id <- post_data("object_component",
                                   list(name = UID,
                                        object = object_id))
}

test_that("new issue creates a new issue on the data registry and associates it with an object", {
  expect_true(is.character(new_issue(severity = 5,
                                     description = UID,
                                     object_issues = list(object_id),
                                     component_issues = list())))
})

UID <- paste0(UID, "1")

test_that("new issue creates a new issue on the data registry and associates it with an object", {
  expect_true(is.character(
    new_issue(severity = 5,
              description = UID,
              object_issues = list(object_id),
              component_issues = list(object_component_id))))
})

UID <- paste0(UID, "2")

test_that("new issue creates a new issue on the data registry and associates it with an object", {
  expect_true(is.character(new_issue(severity = 5,
                                     description = UID,
                                     object_issues = list(),
                                     component_issues = list())))
})

stop_server()

# Issue allows multiple issues with the same details

# test_that("new issue errors with same issue", {
#   expect_message(expect_true(is.character(new_issue(severity = 5,
#                                      description = UID,
#                                      object_issues = list(),
#                                      component_issues = list(),
#                                      key = key))))
# })
