context("Testing new_object_component()")

sleep_time <- 0.5

test_user <- "22"

test_identifier <- sample(1:1000000, 1, replace=TRUE)

UID <- paste0("object component", format(Sys.time(), "%d%m%y%H%M%S"), test_identifier)
UID_1 <- paste0(UID, "1")

run_server()

object_id <- get_entry("object", list(updated_by = test_user))[[1]]$url
if (is.null(object_id))
  object_id <- post_data("object", list(description = UID))

object_id_2 <- get_entry("object", list(updated_by = test_user))[[2]]$url
if(is.null(object_id_2))
  object_id_2 <- post_data("object", list(description = UID_1))

test_that("new_object_component creates a new object component", {
  expect_true(is.character(new_object_component(UID,
                                                object_id,
                                                UID)))
})

test_that("new_object_component creates a new object component", {
  expect_true(is.character(new_object_component(UID_1,
                                                object_id_2)))
})

test_that("new_object_component returns URI if object exists", {
  expect_true(is.character(
    new_object_component(name = UID,
                         object_id = object_id,
                         description = UID)))
})

stop_server()
