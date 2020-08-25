context("Testing get_hash()")

# Download file with known hash

url <- "https://raw.githubusercontent.com/ScottishCovidResponse/SCRCdataAPI/master/data-raw/knownhash.txt"
filename <- "knownhash.txt"
known_hash <- "93c693d7596ae943e79d8fad5021904c6feb4802"

try({utils::download.file(url, filename)}, silent = TRUE)

test_that("get_hash() returns correct hash on a known file", {
  # Skip if file was not downloaded
  skip_if_not(file.exists(filename), "file not downloaded")
  expect_equal(as.character(get_hash(filename)), known_hash)
  expect_equal(get_hash(filename), get_hash(filename))
})

test_that("an error is produced if file does not exist", {
  expect_error(get_hash("unknown_file.txt"))
})


test_that("returned object is class character", {
  expect_error(class(get_hash("unknown_file.txt") == "character"))
})

file.remove(filename)
