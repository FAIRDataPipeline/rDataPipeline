#' This dataset presents the weekly, and year to date, provisional number of
#' deaths associated with coronavirus (COVID-19) alongside the total number
#' of deaths registered in Scotland, broken down by age, sex. (From: https://statistics.gov.scot/data/deaths-involving-coronavirus-covid-19)
#'

library(SCRCdataAPI)
library(SPARQL)

# Download source data
download_source_version(dataset = "scotgov_deaths")

# Process data and generate hdf5 file
sourcefile <- "data-raw/deaths-involving-coronavirus-covid-19.csv"
h5filename <- "deaths-involving-coronavirus-covid-19.h5"
process_scot_gov_deaths(sourcefile = sourcefile,
                        h5filename = h5filename)

openssl::sha256(file(h5filename))
