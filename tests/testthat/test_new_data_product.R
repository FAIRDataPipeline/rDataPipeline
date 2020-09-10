context("Test new_data_product")

# get the token
key <- Sys.getenv("SCRC_API_TOKEN")

sleep_time <- 0.5

test_user <- "22"

datetime <- format(Sys.time(), "%d%m%y%H%M%S")
UID <- paste0("data_product", format(Sys.time(), "%d%m%y%H%M%S_"), datetime)

object_id <- post_data("object",
                         list(description= UID),
                         key)


namespace_id <- post_data("namespace",
                         list(name = UID),
                         key)

test_that("new_data_product posts to data registry", {
  expect_true(is.character(new_data_product(UID,
                                            create_version_number(),
                                            object_id,
                                            namespace_id,
                                            key)))
})

# test_that("new_data_product produces a message if the object exists", {
#   expect_message(expect_true(is.character(new_data_product(UID,
#                                             create_version_number(),
#                                             object_id,
#                                             namespace_id,
#                                             key))))
# })
