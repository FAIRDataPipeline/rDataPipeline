context("Testing new_storage_location()")

sleep_time <- 0.5

root_name <- paste0("test_new_storate_location_",
                    openssl::sha1(x = as.character(Sys.time())))
root <- paste0("https://", root_name, ".com")
hash <- openssl::sha1(x = root_name)
path <- paste0(hash, ".h5")

run_server()

# Register storage root
storage_root_url <- new_storage_root(root = root,
                                     local = TRUE)

test_that("new entry in storage_location returns API URL", {
  expect_true(grepl("storage_location",
                    new_storage_location(path = path,
                                         hash = hash,
                                         public = TRUE,
                                         storage_root_url = storage_root_url)))
})

test_that("existing entry in storage_location returns API URL", {
  expect_true(grepl("storage_location",
                    new_storage_location(path = path,
                                         hash = hash,
                                         public = TRUE,
                                         storage_root_url = storage_root_url)))
})

stop_server()
