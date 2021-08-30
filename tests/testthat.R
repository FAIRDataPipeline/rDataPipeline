library(testthat)
library(rDataPipeline)

Sys.setenv(FDP_endpoint = "http://localhost:8000/api/")
endpoint <- Sys.getenv("FDP_endpoint")

testthat::test_check("rDataPipeline")
