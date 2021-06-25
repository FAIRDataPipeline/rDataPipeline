context("Testing get_entity()")

description <- paste0("test_get_entity_",
                      openssl::sha1(x = as.character(Sys.time())))

run_server()

entity_uri <- post_data("object", list(description = description))

sleep_time <- 0.5

test_that("entity returns as a named list",{
  Sys.sleep(sleep_time)
  expect_silent(get_entity(entity_uri))
  Sys.sleep(sleep_time)
  expect_true(is.list(get_entity(entity_uri)))
  Sys.sleep(sleep_time)
  expect_true(all(c("url", "last_updated") %in%
                    names(get_entity(entity_uri))))
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

stop_server()
