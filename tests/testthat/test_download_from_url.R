context("testing download_from_url")

# Download the file
source_path <- "downloads/file?id=5a9bf61e-7571-45e8-a307-7c1218d5f6b5%2FDatazone2011Lookup.csv"

test_that("download_from_url downloads a file and it can be read", {
  download_from_url(source_root = "http://statistics.gov.scot/",
                  source_path = source_path,
                  path = "geography/scotland/lookup_table",
                  filename = "0.1.0.csv")
  expect_true(file.exists("geography/scotland/lookup_table/0.1.0.csv"))
  df <- read.csv("geography/scotland/lookup_table/0.1.0.csv")
  expect_true(is.data.frame(df))
  file.remove("geography/scotland/lookup_table/0.1.0.csv")
})

test_that("download_from_url downloads a file and it can be read without path", {
  download_from_url(source_root = "http://statistics.gov.scot/",
                    source_path = source_path,
                    filename = "0.1.0.csv")
  expect_true(file.exists("0.1.0.csv"))
  df <- read.csv("0.1.0.csv")
  expect_true(is.data.frame(df))
  file.remove("0.1.0.csv")
})

#todo add nicer error messages
test_that("download_from_url fails correctly", {
  expect_error(download_from_url(source_root = "http://statistics.gov.scot/",
                                 source_path = source_path))
  expect_error(download_from_url(source_root = "http://statistics.gov.scot/"))
  expect_error(download_from_url(source_root = "http://statistics.gov.scot",
                                 source_path = source_path,
                                 filename = "0.1.0.csv"))
  expect_error(download_from_url(source_root = "http://statistics.gov.scot/",
                                 source_path = "/source_path",
                                 filename = "0.1.0.csv"))

})


# Delete the file
unlink("geography", recursive = TRUE)
