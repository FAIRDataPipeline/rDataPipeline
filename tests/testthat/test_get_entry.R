context("Checking get entry")

# Not Run
# key <- read.table("token.txt")
# post_data("object", data = list(description = "TEST OBJECT 001"), key)

object_id <- "32040"
description = "TEST OBJECT 001"

test_that("Check Test object #32040 Exists", {
  expect_success(expect_equal(get_entry("object", query = list(description=description)), get_entry("object", query = list(description=description))))
})

test_that("Check Object has correct fields", {
  object <- get_entry("object", query = list(description=description))
  expect_length(object, length(get_table_readable("object")))
  expect_equal(object$description, description)
  expect_equal(object$url, paste0("https://data.scrc.uk/api/object/", object_id, "/"))
})

test_that("Blank query returns an (last) object", {
  expect_success(expect_equal(get_entry("object", ""), get_entry("object", "")))
})
