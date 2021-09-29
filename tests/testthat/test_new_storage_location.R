context("Testing new_storage_location()")

root_name <- paste0("test_new_storate_location_",
                    openssl::sha1(x = as.character(Sys.time())))
root <- tempdir()
hash <- openssl::sha1(x = root_name)
path <- paste0(hash, ".h5")

endpoint <- Sys.getenv("FDP_endpoint")

# Register storage root
storage_root_url <- new_storage_root(root = root,
                                     local = TRUE,
                                     endpoint = endpoint)

test_that("new entry in storage_location returns API URL", {
  expect_true(grepl("storage_location",
                    new_storage_location(path = path,
                                         hash = hash,
                                         public = TRUE,
                                         storage_root_url = storage_root_url,
                                         endpoint = endpoint)))
})

test_that("existing entry in storage_location returns API URL", {
  expect_true(grepl("storage_location",
                    new_storage_location(path = path,
                                         hash = hash,
                                         public = TRUE,
                                         storage_root_url = storage_root_url,
                                         endpoint = endpoint)))
})
