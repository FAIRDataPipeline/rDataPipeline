context("Testing new_data_product()")

UID <- paste0("test_new_data_product_",
              openssl::sha1(x = as.character(Sys.time())))

run_server()

object_url <- post_data("object",
                        list(description = UID))

namespace_url <- post_data("namespace",
                           list(name = UID))

test_that("new entry in data_product returns API URL", {
  expect_true(grepl("data_product",
                    new_data_product(name = UID,
                                     version = create_version_number(),
                                     object_url = object_url,
                                     namespace_url = namespace_url)))
})

test_that("existing entry in data_product returns API URL", {
  expect_true(grepl("data_product",
                    new_data_product(name = UID,
                                     version = create_version_number(),
                                     object_url = object_url,
                                     namespace_url = namespace_url)))
})

stop_server()
