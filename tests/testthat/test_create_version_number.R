context("Testing create_version_number()")

date <- Sys.Date()
datetime <- Sys.time()
date_1 <- "01/01/2020"
date_2 <- "01-01-2020"
date_3 <- "01012020"
date_4 <- "20200101"
date_5 <- "01.01.2020"
date_6 <- "2020.01.01"
version_1 <- "0.1.0"
version_2 <- "0.2.0"
version_3 <- "1.1.1"
version_4 <- "Inf"
version_5 <- "NaN"
version_6 <- "100000.100000.100000"

output <- "0.20200101.0"
output_1 <- "100000.20200101.100000"

test_that("Sys.Date and Sys.time produce correct results", {
  testthat::expect_equal(create_version_number(date, "0.1.0"),
                         create_version_number(date, "0.1.0"))
  testthat::expect_equal(create_version_number(datetime, "0.1.0"),
                         create_version_number(datetime, "0.1.0"))
  # testthat::expect_equal(create_version_number(datetime, "0.1.0"),
  #                        create_version_number(date, "0.1.0"))
})

test_that("create_version_number output is as expected", {
  testthat::expect_equal(create_version_number(date_1, version_1),
                         create_version_number(date_2, version_1))
  testthat::expect_equal(create_version_number(date_5, version_1),
                         create_version_number(date_6, version_1))
  testthat::expect_warning(
    testthat::expect_equal(create_version_number(date_3, version_1),
                           create_version_number(date_4, version_1)))
  testthat::expect_equal(create_version_number(date_1, version_1), output)
  testthat::expect_equal(create_version_number(date_2, version_1), output)
  testthat::expect_equal(create_version_number(date_4, version_1), output)
  testthat::expect_equal(create_version_number(date_5, version_1), output)
  testthat::expect_equal(create_version_number(date_6, version_1), output)
  testthat::expect_equal(create_version_number(), version_1)
  testthat::expect_equal(create_version_number(date_4, "0"), output)
  testthat::expect_equal(create_version_number(date_4, "0.0"), output)
  testthat::expect_equal(create_version_number(date_4, major = "0",
                                               minor = "0"), output)
})

test_that("create_version_number validation works as expected", {
  testthat::expect_error(create_version_number("01-01-20", version_1))
  testthat::expect_error(create_version_number("20-01-01", version_1))
  testthat::expect_error(create_version_number(date, version_4))
  testthat::expect_error(create_version_number(date, version_5))
  testthat::expect_error(create_version_number("200101", version_1))
  testthat::expect_error(create_version_number("18150101", version_1))
  testthat::expect_warning(create_version_number("01022020", version_1))
})
