context("Testing get_entry()")

key <- Sys.getenv("SCRC_API_TOKEN")

sleep_time <- 0.5

formatted_date <- format(Sys.time(), "%d%m%y%H%M%S")

test_identifier <- sample(1:1000000, 1, replace=TRUE)

UID <- paste0("Object Test ", formatted_date, test_identifier)

object_id <- post_data("object", data = list(description = UID), key)
object_id <- unlist(clean_query(object_id))
description <- UID
Sys.sleep(sleep_time)

test_that("Check Test object Exists", {
  expect_silent(
    expect_equal(get_entry("object", query = list(description=description)),
                 get_entry("object", query = list(description=description)))
    )
})

Sys.sleep(sleep_time)
test_that("Unknown Table causes and error", {
  expect_error(get_entry("unknown", list()))
})

Sys.sleep(sleep_time)
test_that("invalid query causes and error", {
  expect_error(get_entry("object", list(url="")))
  expect_error(get_entry("object", "query"))
  expect_error(get_entry("object", 5))
  expect_error(get_entry("object", Inf))
  expect_error(get_entry("object", NaN))
  expect_error(get_entry("object", as.data.frame()))
  expect_error(get_entry("object", NULL))
})

test_that("query = \"\" produces a warning", {
  Sys.sleep(sleep_time)
  expect_warning(get_entry("object", "" ))
})

Sys.sleep(sleep_time)
test_that("multiple matches returns a list of more than one object", {
  post_data("object", data = list(description = description), key)
  Sys.sleep(sleep_time)
  expect_true(length(get_entry("object", list(description = description))) > 1)
})

Sys.sleep(sleep_time)
test_that("Blank query returns list of objects", {
  expect_silent(expect_true(is.list(get_entry("object", list()))))
})
Sys.sleep(sleep_time)

test_that("Check Objects have correct fields", {
  object <- get_entry("object", query = list(description=description))

  expect_equal(lapply(object, function(x) x$description) %>%
                 unlist() %>% unique(),
               description)
  expect_equal(lapply(object, length) %>% unlist() %>% unique(), length(get_table_readable("object", key)$field))
})

