# library(testthat)
# library(rFDP)


Sys.setenv(FDP_endpoint = "http://localhost:8000/api/")
# Sys.setenv(FDP_endpoint = "https://data.scrc.uk/api/")

# testthat::test_check("rFDP")

endpoint <- Sys.getenv("FDP_endpoint")

fair_init(family_name = "Mitchell",
          given_name = "Sonia",
          orcid = "https://orcid.org/0000-0003-1536-2066",
          organisation = c("University of Glasgow",
                           "Boydorr Centre for Population and Ecosystem Health"),
          endpoint = endpoint)
