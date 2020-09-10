context("Test new_object_component")

# get the token
key <- Sys.getenv("SCRC_API_TOKEN")

sleep_time <- 0.5

test_user <- "22"

UID <- paste0("object_component_Test_OBJECT_", format(Sys.time(), "%d%m%y%H%M%S"))
UID_1 <- paste0(UID, "_1")

object_id <- get_entry("object", list(updated_by = test_user))[[1]]$url

if(is.null(object_id)){
  object_id <- post_data("object",
                         list(name = UID),
                         key)
}

object_id_2 <- get_entry("object", list(updated_by = test_user))[[2]]$url

if(is.null(object_id_2)){
  object_id_2 <- post_data("object",
                         list(name = UID_1),
                         key)
}

test_that("new_object_component creates a new object component", {
  expect_true(is.character(new_object_component(UID,
                                                object_id,
                                                UID,
                                                key)))
})

test_that("new_object_component creates a new object component", {
  expect_true(is.character(new_object_component(UID_1,
                                                object_id_2,
                                                key = key)))
})

test_that("new_object_component produces message if object exists", {
  expect_message(expect_true(is.character(new_object_component(UID,
                                                object_id,
                                                UID,
                                                key))))
})
