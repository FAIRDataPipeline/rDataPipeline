context("Testing new_issue()")

uid <- as.character(random_hash())

endpoint <- Sys.getenv("FDP_endpoint")

object_url <- post_data(table = "object",
                        data = list(name = uid),
                        endpoint = endpoint)

object_component_url <- post_data(table = "object_component",
                                  data = list(name = uid,
                                              object = object_url),
                                  endpoint = endpoint)

test_that("new issue creates an issue and associates it with a component", {
  tmp <- new_issue(severity = 5,
                   description = uid,
                   component_issues = list(),
                   endpoint = endpoint)
  expect_true(grepl(endpoint, tmp))
})

test_that("new issue creates an issue and associates it with a component", {
  tmp <- new_issue(severity = 5,
                   description = uid,
                   component_issues = list(object_component_url),
                   endpoint = endpoint)
  expect_true(grepl(endpoint, tmp))
})
