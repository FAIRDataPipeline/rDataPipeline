context("Testing get_entity()")

description <- paste0("test_get_entity_",
                      openssl::sha1(x = as.character(Sys.time())))

endpoint <- Sys.getenv("FDP_endpoint")

entity_url <- post_data("object", list(description = description),
                        endpoint = endpoint)

test_that("entity returns as a named list", {
  expect_silent(get_entity(entity_url))
  expect_true(is.list(get_entity(entity_url)))
  expect_true(all(c("url", "last_updated") %in%
                    names(get_entity(entity_url))))
})

test_that("invalid table produces and error", {
  expect_error(get_entity("unknown", entity_id))
})

test_that("invalid entity_id produces and error", {
  expect_error(get_entity("object", "NotANumber"))
  expect_error(get_entity("object", list(something = "something")))
  expect_error(get_entity("object", NaN))
  expect_error(get_entity("object", Inf))
  expect_error(get_entity("object", -Inf))
})
