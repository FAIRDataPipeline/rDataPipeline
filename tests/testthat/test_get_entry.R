context("Testing get_entry()")

sleep_time <- 0.5
endpoint <- "https://data.scrc.uk/api/"
description <- paste0("test_get_entry_",
                      openssl::sha1(x = as.character(Sys.time())))

run_server()

object_uri <- post_data("object", data = list(description = description),
                        endpoint = endpoint)
object_id <- extract_id(object_uri)
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
  # expect_error(get_entry("object", 5))
  # expect_error(get_entry("object", Inf))
  # expect_error(get_entry("object", NaN))
  expect_error(get_entry("object", as.data.frame()))
  # expect_error(get_entry("object", NULL))
  # expect_error(get_entry("object", "" ))
})

Sys.sleep(sleep_time)

test_that("multiple matches returns a list of more than one object", {
  post_data("object", data = list(description = description), endpoint = endpoint)
  Sys.sleep(sleep_time)
  expect_true(length(get_entry("object", list(description = description))) > 1)
})

Sys.sleep(sleep_time)

test_that("Check Objects have correct fields", {
  object <- get_entry("object", query = list(description = description))

  expect_equal(lapply(object, function(x) x$description) %>%
                 unlist() %>% unique(),
               description)
  expect_equal(lapply(object, length) %>% unlist() %>% unique(),
               length(get_table_readable("object")$field))
})

stop_server()
