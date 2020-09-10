context("Test new_data_product")

# get the token
key <- Sys.getenv("SCRC_API_TOKEN")

sleep_time <- 0.5

test_user <- "22"

UID <- paste0("data_product_Test_OBJECT_", format(Sys.time(), "%d%m%y%H%M%S"))

object_id <- get_entry("object", list(updated_by = test_user))[[1]]$url

namespace_id <- get_entry("namespace", list(updated_by = test_user))[[1]]$url

if(is.null(object_id)){
  object_id <- post_data("object",
                         list(name = UID),
                         key)
}

if(is.null(namespace_id)){
  namespace_id <- post_data("namespace",
                         list(name = UID),
                         key)
}

test_that("new_data_product posts to data registry", {
  expect_true(is.character(new_data_product(UID,
                                            create_version_number(),
                                            object_id,
                                            namespace_id,
                                            key)))
})

test_that("new_data_product produces a message if the object exists", {
  expect_message(expect_true(is.character(new_data_product(UID,
                                            create_version_number(),
                                            object_id,
                                            namespace_id,
                                            key))))
})
