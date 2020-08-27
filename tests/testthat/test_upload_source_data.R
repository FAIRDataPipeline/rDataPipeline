context("Testing upload_source_data")

key <- Sys.getenv("SCRC_API_TOKEN")

test_user <- "22"

formatted_date <- format(Sys.time(), "%d%m%y%H%M%S")

UID <- paste0("TEST_", formatted_date)

source_id <- get_entry("source", list(updated_by = test_user))[[1]]$url

path = paste0("https://", UID)

filename <- paste0("test_table_", UID, ".h5")
df <- data.frame(a = 1:2, b = 3:4)
component <- "level"

create_table(filename = filename,
             component = component,
             df = df)

if(is.null(source_id)){
  source_id <- post_data("source",
                         list(name = UID, abbreviation = formatted_date),
                         key)
}

original_root_id <- post_data("storage_root",
                              list(name = path, root = path),
                              key)

storage_root_id <- post_data("storage_root",
                             list(name = UID, root = UID),
                             key)

test_that("upload_source_data returns id", {
  expect_true(is.list(upload_source_data(UID,
                                         source_id,
                                         original_root_id,
                                         filename,
                                         primary_not_supplement = T,
                                         filename,
                                         storage_root_id,
                                         path,
                                         format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                         create_version_number(),
                                         key)))
})

test_that("upload_source_data errors without key", {
  expect_error(upload_source_data(UID,
                                  source_id,
                                  original_root_id,
                                  filename,
                                  primary_not_supplement = T,
                                  filename,
                                  storage_root_id,
                                  path,
                                  format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                  create_version_number(),
                                  key = NULL))
})

test_that("upload_source_data errors with invalid file", {
  expect_error(upload_source_data(UID,
                                  source_id,
                                  original_root_id,
                                  filename,
                                  primary_not_supplement = T,
                                  local_path = "unknown_file.h5",
                                  storage_root_id,
                                  path,
                                  format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                  create_version_number(),
                                  key = NULL))
})


file.remove(filename)

