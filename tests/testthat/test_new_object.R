context("Testing new_object()")

sleep_time <- 0.5

test_user <- "22"

test_identifier <- sample(1:1000000, 1, replace=TRUE)

datetime <- format(Sys.time(), "%d%m%y%H%M%S")

UID <- paste0("object ", datetime, test_identifier)
abbreviation <- paste0("obj ", datetime, test_identifier)
UID_ <- paste0("external_object_", datetime, test_identifier)
path <- paste0(UID_, ".h5")
path_uri <- paste0("https://", path, ".com")
hash <- paste0(Sys.time(), "%d%m%y%H%M%S")

run_server()

storage_root_id <- get_entry("storage_root", list(updated_by = test_user))[[1]]$url


if(is.null(storage_root_id)){
  storage_root_id <- post_data("storage_root",
                               list(name = UID,
                                    root = path_uri))
}

store_id <- post_data("storage_location",
                      list(path = path,
                           hash = hash,
                           storage_root = storage_root_id))


test_that("new_object creates a new object", {
  expect_true(is.character(new_object("",
                                      description = UID)))
})

test_that("new_object creates a new object", {
  expect_true(is.character(new_object(store_id,
                                      description = UID)))
})

test_that("new_object creates a new object", {
  expect_true(is.character(new_object("",
                                      description = "")))
})

test_that("new_object returns URI when the object exists", {
  expect_true(is.character(new_object(store_id,
                                      description = UID)))
})

stop_server()
