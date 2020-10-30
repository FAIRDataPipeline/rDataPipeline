context("Testing github_files")

test_that("github_files errors if repo does not exist",{
  expect_error(SCRCdataAPI:::github_files("thisshouldnotexist404errorbleep"))
})

test_that("github_files works on SCRCdataAPI",{
  expect_true(is.character(SCRCdataAPI:::github_files("ScottishCovidResponse/SCRCdataAPI")))
  expect_true(any(grepl("DESCRIPTION", SCRCdataAPI:::github_files("ScottishCovidResponse/SCRCdataAPI"))))
})
