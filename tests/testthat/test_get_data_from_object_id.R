context("testing get_data_from_object_id")

data_dir <- "data-raw"
root <- "ftp://boydorr.gla.ac.uk/scrc/"
sonia <- 3

storage_root <- get_entry("storage_root", list(root = root))[[1]]
storage_root_id <- basename(storage_root$url)

#storage_location <- storage_root$locations[[1]]

storage_locations <- get_entry("storage_location", list(updated_by = sonia,
                                                       storage_root = storage_root_id))

storage_locations <- rev(storage_locations)

storage_location <- ""

for(i in seq_along(storage_locations)){
  if(grepl("*.h5$", storage_locations[[i]]$path)){
    storage_location <- storage_locations[[i]]$url
    break
  }
}

storage_location_id <- basename(storage_location)
object <- get_entry("object", list(storage_location = storage_location_id))[[1]]
object_id <- basename(object$url)

downloaded_object <- list()

test_that("get_data_from_object_id correctly downloads .H5",{
  skip_if(storage_location_id == "")
  skip_if(is.na(storage_location_id))
  downloaded_object <- get_data_from_object_id(object_id, data_dir = "data-raw")
  expect_true(is.list(downloaded_object))
  expect_true(file.exists(downloaded_object$downloaded_to))
})

unlink(data_dir, recursive = TRUE)

