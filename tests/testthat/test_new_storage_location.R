context("Test new_storage_location")

# get the token
key <- Sys.getenv("SCRC_API_TOKEN")

sleep_time <- 0.5

test_user <- "22"

test_identifier <- sample(1:100, 1, replace=TRUE)

UID <- paste0("storage location ", format(Sys.time(), "%d%m%y%H%M%S"))
UID_ <- paste0("storagelocation", format(Sys.time(), "%d%m%y%H%M%S"))
path <- paste0(UID_, ".h5")
path_uri <- paste0("https://", UID_, ".com")
hash <- paste0(Sys.time(), "%d%m%y%H%M%S")

storage_root_id <- get_entry("storage_root", list(updated_by = test_user))[[1]]$url

if(is.null(storage_root_id)){
  storage_root_id <- post_data("storage_root",
                               list(name = UID,
                                    root = path_uri),
                               key)
}

test_that("new_storage_location creates a new storage location", {
  expect_true(is.character(new_storage_location(path,
                                                hash,
                                                storage_root_id,
                                                key)))
})

test_that("new_storage_location produces a message if the location already exists", {
  expect_message(expect_true(is.character(new_storage_location(path,
                                                hash,
                                                storage_root_id,
                                                key))))
})
