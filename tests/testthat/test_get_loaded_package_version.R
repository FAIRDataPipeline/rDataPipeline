context("testing get_loaded_package_version")

test_that("get_loaded_package_version returns a character", {
  expect_silent(get_loaded_package_version())
  expect_true(is.list(get_loaded_package_version()))
})

test_that("get_loaded_package_version is co-orcable to version", {
  expect_silent(as.package_version(get_loaded_package_version()))
})

test_that("null package produces and error",{
  expect_error(get_loaded_package_version(NULL))
})

test_that("unknown package produces and error", {
  expect_error(get_loaded_package_version("Uknown"))
})
