context("Testing new_object_component()")

UID <- paste0("test_new_object_component_",
              openssl::sha1(x = as.character(Sys.time())))
UID2 <- paste0(UID, "1")

endpoint <- Sys.getenv("FDP_endpoint")

object_url <- post_data("object", list(description = UID), endpoint = endpoint)
object_url2 <- post_data("object", list(description = UID2),
                         endpoint = endpoint)

test_that("new entry in object_component returns API URL", {
  expect_true(grepl("object_component",
                    new_object_component(object_url = object_url,
                                         name = UID,
                                         description = UID,
                                         endpoint = endpoint)))
})

test_that("existing entry in object_component returns API URL", {
  expect_true(grepl("object_component",
                    new_object_component(object_url = object_url,
                                         name = UID,
                                         description = UID,
                                         endpoint = endpoint)))
})
