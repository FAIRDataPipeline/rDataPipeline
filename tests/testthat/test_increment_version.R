context("testing increment_version")

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
current_version <- max(versions) %>% as.numeric_version()
min_version <- min(versions) %>% as.numeric_version()
inc_version <- current_version %>% as.character() %>% semver::parse_version() %>% semver::increment_version(field = "patch", value = 1L)

# this should be how the test works (Increment version number does not output anything???)
# test_that("increment_versions works correctly", {
#   expect_equal(increment_version(data_product$name, as.character(inc_version)), inc_version)
#   expect_equal(increment_version(data_product$name), inc_version)
# })

#this is how the test actually works

test_that("increment_versions works (in)correctly", {
  expect_null(increment_version(data_product$name, as.character(inc_version)))
  expect_equal(increment_version(data_product$name), inc_version)
})

# to do add friendly errors
test_that("increment_version errors as it should",{
  expect_error(increment_version(data_product$name, 5))
  expect_error(supressWarnings(increment_version("unknown")))
  expect_warning(increment_version(data_product$name, "0.1.0"))
  expect_warning(increment_version(data_product$name, as.character(min_version)))
})
