context("Checking get entry")

key <- Sys.getenv("SCRC_API_TOKEN")

formatted_date <- format(Sys.time(), "%d%m%y%H%M%S")
UID <- paste0("TEST OBJECT ", formatted_date)
object_id <- post_data("object", data = list(description = UID), key)
object_id <- unlist(clean_query(object_id))
description <- UID

test_that("Check Test object Exists", {
  expect_silent(
    expect_equal(get_entry("object", query = list(description=description)),
                 get_entry("object", query = list(description=description)))
    )
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

test_that("multiple matches returns a list of more than one object", {
  post_data("object", data = list(description = description), key)
  expect_true(length(get_entry("object", list(description = description))) > 1)
})

test_that("Check Objects have correct fields", {
  object <- get_entry("object", query = list(description=description))

  expect_equal(lapply(object, function(x) x$description) %>%
                 unlist() %>% unique(),
               description)
  expect_equal(lapply(object, length) %>% unlist() %>% unique(), length(get_table_readable("object", key)))
})

