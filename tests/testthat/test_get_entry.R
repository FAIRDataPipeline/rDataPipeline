context("Testing get_entry()")

description <- paste0("test_get_entry_",
                      openssl::sha1(x = as.character(Sys.time())))

endpoint <- Sys.getenv("FDP_endpoint")

object_uri <- post_data("object", data = list(description = description),
                        endpoint = endpoint)
object_id <- extract_id(object_uri, endpoint = endpoint)

test_that("Check Test object Exists", {
  expect_silent(
    expect_equal(get_entry("object", query = list(description = description)),
                 get_entry("object", query = list(description = description)))
    )
})

test_that("Unknown Table causes and error", {
  expect_error(get_entry("unknown", list()))
})

test_that("invalid query causes and error", {
  expect_error(get_entry("object", list(url = "")))
  expect_error(get_entry("object", "query"))
  # expect_error(get_entry("object", 5))
  # expect_error(get_entry("object", Inf))
  # expect_error(get_entry("object", NaN))
  expect_error(get_entry("object", as.data.frame()))
  # expect_error(get_entry("object", NULL))
  # expect_error(get_entry("object", "" ))
})

test_that("multiple matches returns a list of more than one object", {
  post_data("object", data = list(description = description),
            endpoint = endpoint)
  expect_true(length(get_entry("object", list(description = description))) > 1)
})

test_that("Check Objects have correct fields", {
  object <- get_entry(table = "object",
                      query = list(description = description),
                      endpoint = endpoint)

  expect_equal(lapply(object, function(x) x$description) %>%
                 unlist() %>%
                 unique(),
               description)
  expect_equal(lapply(object, length) %>%
                 unlist() %>%
                 unique(),
               length(get_table_readable(table = "object",
                                         endpoint = endpoint)$field))
})
