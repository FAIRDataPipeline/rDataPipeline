context("testing download_external_object")

data_dir <- "data-raw"
sonia <- 3

objects <- get_entry("object", list(updated_by = sonia))

objects <- rev(objects)

external_object <- ""

for(i in seq_along(objects)){
  if(!is.null(objects[[i]]$external_object)){
    object <- objects[[i]]
    break
  }
}

external_object_id <- basename(object$external_object)
external_object <- get_entity("external_object", external_object_id)
external_objects <- get_entry("external_object", list(doi_or_unique_name = external_object$doi_or_unique_name))

versions <- do.call(Map, c(f=list, external_objects))$version %>% as.numeric_version()
version <- as.numeric_version(max(versions))
version <- as.character(version)

external_object <- external_objects[[which(versions == version)]]
object <- get_entity("object", basename(external_object$object))
storage_location_id <- basename(object$storage_location)
storage_location <- get_entity("storage_location", storage_location_id)

components <- object$components
component_id <- ""
component <- list()
if(!is.null(components)){
  component_id <- basename(components[[1]])
  component <- get_entity("object_component", component_id)
}

test_that("download_external_object downloads an object", {
  downloaded_object <- download_external_object(external_object$doi_or_unique_name, data_dir)
  expect_true(is.list(downloaded_object))
  expect_true(file.exists(paste0(data_dir, "/", basename(storage_location$path))))
  skip_if(component_id == "")
  expect_true(component$name %in% downloaded_object$components)
})

unlink(data_dir, recursive = TRUE)

