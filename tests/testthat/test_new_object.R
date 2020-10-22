context("Testing new_object()")

# get the token
key <- Sys.getenv("SCRC_API_TOKEN")

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

storage_root_id <- get_entry("storage_root", list(updated_by = test_user))[[1]]$url


if(is.null(storage_root_id)){
  storage_root_id <- post_data("storage_root",
                               list(name = UID,
                                    root = path_uri),
                               key)
}

store_id <- post_data("storage_location",
                                 list(path = path,
                                      hash = hash,
                                      storage_root = storage_root_id),
                                 key)


test_that("new_object creates a new object", {
  expect_true(is.character(new_object("",
                                      description = UID,
                                      key)))
})

test_that("new_object creates a new object", {
  expect_true(is.character(new_object(store_id,
                                      description = UID,
                                      key)))
})

test_that("new_object creates a new object", {
  expect_true(is.character(new_object("",
                                      description = "",
                                      key)))
})

test_that("new_object produces a message when object exists", {
  expect_message(expect_true(is.character(new_object(store_id,
                                      description = UID,
                                      key))))
})
