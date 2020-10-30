context("testing get_package_info")

test_that("get_package_info returns a list",{
  expect_true(is.list(get_package_info("ScottishCovidResponse/SCRCdataAPI", "DESCRIPTION")))
  expect_equivalent(get_package_info("ScottishCovidResponse/SCRCdataAPI", "DESCRIPTION")$submission_script, "DESCRIPTION")
})

###############################################################################
##  No effective way to test this as version may not match between branches  ##
###############################################################################

# test_that("get_package_info works with package",{
#   expect_true(is.list(get_package_info("ScottishCovidResponse/SCRCdataAPI", "DESCRIPTION", "SCRCdataAPI")))
#   expect_equivalent(get_package_info("ScottishCovidResponse/SCRCdataAPI", "DESCRIPTION")$submission_script, "DESCRIPTION", "SCRCdataAPI")
# })
