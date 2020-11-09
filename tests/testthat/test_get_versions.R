context("testing get_version_numbers")

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
versions <- do.call(Map, c(f=list, data_products))$version %>% as.character()

test_that("get_version_numbers works with a data product",{
  expect_equal(get_version_numbers(data_product$name), versions)
})

#todo add friendly errors
test_that("get_version_numbers fail appropriately", {
  expect_error(get_version_numbers())
  expect_message(get_version_numbers("unknown"))
})
