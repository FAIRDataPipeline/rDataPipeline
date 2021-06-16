context("Testing new_external_object()")

UID <- paste0("test_new_external_object_",
              openssl::sha1(x = as.character(Sys.time())))
path <- paste0(UID, ".h5")
path_url <- paste0("https://", path)
hash <- sha1(UID)

run_server()

object_url <- post_data("object",
                        list(description = UID))

storage_root_url <- post_data("storage_root",
                              list(name = UID,
                                   root = path_url))

original_store_url <- post_data("storage_location",
                                list(path = path,
                                     hash = hash,
                                     storage_root = storage_root_url))

test_that("New external object creates an external object with all fields", {
  expect_true(grepl("external_object",
                    new_external_object(doi_or_unique_name = UID,
                                        primary_not_supplement = TRUE,
                                        release_date = Sys.time(),
                                        title = UID,
                                        description = UID,
                                        object_url = object_url,
                                        original_store_url = original_store_url)))
})

stop_server()
