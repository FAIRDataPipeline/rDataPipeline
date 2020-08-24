context("Test get_startup_message")

test_that("Start up message returns a message", {
  expect_true(is.character(get_startup_message("ScottishCovidResponse/SCRCdataAPI", "SCRCdataAPI")))
})
