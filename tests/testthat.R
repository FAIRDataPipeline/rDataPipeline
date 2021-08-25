library(testthat)
library(rDataPipeline)

Sys.setenv(FDP_endpoint = "http://localhost:8000/api/")
endpoint <- Sys.getenv("FDP_endpoint")
run_server()

rDataPipeline::fair_init(
  name = "Sonia Mitchell",
  identifier = "https://orcid.org/0000-0003-1536-2066",
  endpoint = endpoint)

testthat::test_check("rDataPipeline")
