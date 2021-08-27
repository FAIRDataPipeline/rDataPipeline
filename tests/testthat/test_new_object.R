context("Testing new_object()")

UID <- paste0("test_new_object_", openssl::sha1(x = as.character(Sys.time())))
path <- paste0(UID, ".h5")
path_url <- paste0("https://", path, ".com")
hash <- paste0(Sys.time(), "%d%m%y%H%M%S")

endpoint <- Sys.getenv("FDP_endpoint")

storage_root_url <- post_data("storage_root",
                              list(name = UID,
                                   root = path_url),
                              endpoint = endpoint)

store_url <- post_data("storage_location",
                       list(path = path,
                            hash = hash,
                            storage_root = storage_root_url),
                       endpoint = endpoint)

test_that("new entry in object returns API URL", {
  expect_true(grepl("object", new_object("",
                                         description = UID,
                                         endpoint = endpoint)))
})

test_that("new entry in object returns API URL", {
  expect_true(grepl("object", new_object(store_url,
                                         description = UID,
                                         endpoint = endpoint)))
})

test_that("new entry in object returns API URL", {
  expect_true(grepl("object", new_object("",
                                         description = "",
                                         endpoint = endpoint)))
})

test_that("existing entry in object returns API URL", {
  expect_true(grepl("object", new_object(store_url,
                                         description = UID,
                                         endpoint = endpoint)))
})
