context("Testing github_files")

test_that("github_files errors if repo does not exist",{
  expect_error(SCRCdataAPI:::github_files("thisshouldnotexist404errorbleep"))
})

test_that("github_files works on rFDP",{
  expect_true(is.character(
    rFDP:::github_files("FAIRDataPipeline/rFDP")))
  expect_true(any(grepl(
    "DESCRIPTION", rFDP:::github_files("FAIRDataPipeline/rFDP"))))
})
