context("testing download_data_product")

data_dir <- "data-raw"
sonia <- 3

objects <- get_entry("object", list(updated_by = sonia))

objects <- rev(objects)

external_object <- ""

for(i in seq_along(objects)){
  if(!is.null(objects[[i]]$data_product)){
    object <- objects[[i]]
    break
  }
}

data_product_id <- basename(object$data_product)
data_product <- get_entity("data_product", data_product_id)
data_products <- get_entry("data_product", list(name = data_product$name))

versions <- do.call(Map, c(f=list, data_products))$version %>% as.numeric_version()
version <- as.numeric_version(max(versions))
version <- as.character(version)

data_product <- data_products[[which(versions == version)]]
object <- get_entity("object", basename(data_product$object))
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
  downloaded_object <- download_data_product(data_product$name, data_dir)
  expect_true(is.list(downloaded_object))
  expect_true(file.exists(paste0(data_dir, "/", basename(storage_location$path))))
  skip_if(component_id == "")
  expect_true(component$name %in% downloaded_object$components)
})

unlink(data_dir, recursive = TRUE)
