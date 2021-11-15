context("Testing get_file_hash()")

# Write file
filename <- file.path(tempdir(), "hash.txt")
cat(1, file = filename)
hash <- "356a192b7913b04c54574d18c28d46e6395428ab"

test_that("get_file_hash() returns correct hash on a known file", {
  expect_equal(as.character(get_file_hash(filename)), hash)
})

test_that("an error is produced if file does not exist", {
  expect_error(get_file_hash("unknown_file.txt"))
})


test_that("returned object is class character", {
  expect_error(class(get_file_hash("unknown_file.txt") == "character"))
})
