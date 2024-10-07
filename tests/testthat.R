library(testthat)
library(rDataPipeline)

# Don't run tests on CRAN's isolated test rig, since they depend on a local
# registry and CLI being installed and will therefore fail.

if (Sys.getenv("_RUN_TESTS_") != "") {
  Sys.setenv(FDP_endpoint = "http://127.0.0.1:8000/api/")
  endpoint <- Sys.getenv("FDP_endpoint")
  testthat::test_check("rDataPipeline")
}
