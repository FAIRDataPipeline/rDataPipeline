context("Checking get entry")

key <- Sys.getenv("SCRC_API_TOKEN")
object_id <- post_data("object", data = list(description = "TEST OBJECT 001"), key)
object_id <- unlist(clean_query(object_id))
description <- "TEST OBJECT 001"

test_that("Check Test object Exists", {
  expect_success(
    expect_equal(get_entry("object", query = list(description=description)),
                 get_entry("object", query = list(description=description)))
    )
})

test_that("Check Object has correct fields", {
  object <- get_entry("object", query = list(description=description))

  expect_equal(lapply(object, function(x) x$description) %>%
                 unlist() %>% unique(),
               description)
  expect_equal(lapply(object, length) %>% unlist() %>% unique(), 14)
})

test_that("Blank query returns an (last) object", {
  expect_success(expect_equal(get_entry("object", list()), get_entry("object", list())))
})

test_that("null query returns last object", {
  expect_success(expect_equal(get_entry("object", list()), get_entry("object")))
  expect_success(expect_equal(get_entry("object", list()), get_entry("object"), NULL))
})

test_that("Unknown Table causes and error", {
  expect_error(get_entry("unknown", list()))
})

test_that("invalid query causes and error", {
  expect_error(get_entry("object", list(url="")))
  expect_error(get_entry("object", "query"))
  expect_error(get_entry("object", 5))
  expect_error(get_entry("object", Inf))
  expect_error(get_entry("object", NaN))
  expect_error(get_entry("object", as.data.frame()))
})

test_that("query = \"\" produces a warning", {
  expect_warning(get_entry("object", "" ))
})

#run with key
# test_that("object has correct number of fields", {
#   expect_length(object, length(get_table_readable("object")))
# })


