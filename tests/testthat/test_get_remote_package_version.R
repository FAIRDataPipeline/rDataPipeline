context("Testing get_remote_package_version()")

test_that("get_remote_package_version returns a character", {
  expect_silent(get_remote_package_version())
  expect_true(is.character(get_remote_package_version()))
})

test_that("get_remote_package_version works on SCRCdata", {
  expect_true(is.character(get_remote_package_version("ScottishCovidResponse/SCRCdata")))
})

test_that("get_remote_package_version is co-orcable to version", {
  expect_silent(as.package_version(get_remote_package_version()))
})

test_that("null package produces and error",{
  expect_error(get_remote_package_version(NULL))
})

test_that("unknown package produces an error", {
  expect_error(get_remote_package_version("Uknown"))
})
