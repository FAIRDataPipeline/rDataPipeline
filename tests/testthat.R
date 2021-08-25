# library(testthat)
library(rDataPipeline)


Sys.setenv(FDP_endpoint = "http://localhost:8000/api/")
# Sys.setenv(FDP_endpoint = "https://data.scrc.uk/api/")
# testthat::test_check("rDataPipeline")

endpoint <- Sys.getenv("FDP_endpoint")
# if (grepl("localhost", endpoint)) run_server()

# rFDP::fair_init(
#   name = "Sonia Mitchell",
#   identifier = "https://orcid.org/0000-0003-1536-2066",
#   endpoint = endpoint)
