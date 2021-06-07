context("Testing new_external_object()")

sleep_time <- 0.5

test_user <- "22"

test_identifier <- sample(1:1000000, 1, replace=TRUE)

datetime <- format(Sys.time(), "%d%m%y%H%M%S")

UID <- paste0("external object ", datetime, test_identifier)
UID_name <- paste0("external object ", datetime, test_identifier)
abbreviation <- paste0("EO ", datetime, test_identifier)
path <- paste0(UID_name, ".h5")
path_uri <- paste0("https://", path)
hash <- sha1(UID)

run_server()

object_id <- post_data("object",
                       list(description = UID))


source_id <- get_entry("source", list(updated_by = test_user))[[1]]$url
original_store_id <- get_entry("storage_location", list(updated_by = test_user))[[1]]$url
storage_root_id <- get_entry("storage_root", list(updated_by = test_user))[[1]]$url

if(is.null(source_id)){
  source_id <- post_data("source",
                         list(name = UID,
                              abbreviation = abbreviation))
}

if(is.null(storage_root_id)){
  storage_root_id <- post_data("storage_root",
                               list(name = UID,
                                    root = path_uri))
}

if(is.null(original_store_id)){
  original_store_id <- post_data("storage_location",
                                 list(path = path,
                                      hash = hash,
                                      storage_root = storage_root_id))
}

test_that("New external object creates an external object with all fields", {
  expect_true(is.character(new_external_object(UID,
                                               TRUE,
                                               Sys.time(),
                                               UID,
                                               UID,
                                               create_version_number(),
                                               object_id,
                                               source_id,
                                               original_store_id)))
})

object_id <- post_data("object",
                       list(description = UID))

test_that("New external object creates an external object with required fields", {
  expect_true(is.character(
    new_external_object(UID,
                        release_date = Sys.time(),
                        title = UID,
                        version = create_version_number(Sys.time(), "0.2.0"),
                        object_id = object_id,
                        source_id =source_id)))
})

stop_server()

# test_that("New external object throws error if the object_id has been used before", {
#   expect_error(is.character(
#     new_external_object(UID,
#                         release_date = Sys.time(),
#                         title = UID,
#                         version = create_version_number(Sys.time(), "0.2.0"),
#                         object_id = object_id,
#                         source_id = source_id)))
# })
