context("testing get entity")

key <- Sys.getenv("SCRC_API_TOKEN")

entity_id <- basename(post_data("object", list(description = Sys.time()), key))

sleep_time <- 0.5

test_that("entity returns as list",{
  Sys.sleep(sleep_time)
  expect_silent(get_entity("object", entity_id))
  Sys.sleep(sleep_time)
  expect_true(is.list(get_entity("object", entity_id)))
})

test_that("invalid table produces and error",{
  Sys.sleep(sleep_time)
  expect_error(get_entity("unknown", entity_id))
})

test_that("invalid entity_id produces and error",{
  expect_error(get_entity("object", "NotANumber"))
  Sys.sleep(sleep_time)
  expect_error(get_entity("object", list(something = "something")))
  Sys.sleep(sleep_time)
  expect_error(get_entity("object", NaN))
  Sys.sleep(sleep_time)
  expect_error(get_entity("object", Inf))
  Sys.sleep(sleep_time)
  expect_error(get_entity("object", -Inf))
})

