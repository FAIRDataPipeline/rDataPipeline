context("Testing github_files")

test_that("github_files errors if repo does not exist",{
  expect_error(rDataPipeline:::github_files("thisshouldnotexist404errorbleep"))
})

test_that("github_files works on rDataPipeline",{
  expect_true(is.character(
    rDataPipeline:::github_files("FAIRDataPipeline/rDataPipeline")))
  expect_true(any(grepl(
    "DESCRIPTION", rDataPipeline:::github_files("FAIRDataPipeline/rDataPipeline"))))
})
