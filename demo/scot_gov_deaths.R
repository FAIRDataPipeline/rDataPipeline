#' This dataset presents the weekly, and year to date, provisional number of
#' deaths associated with coronavirus (COVID-19) alongside the total number
#' of deaths registered in Scotland, broken down by age, sex.
#'
#' Sourced from: statistics.gov.scot
#'
#' More information can be found here:
#' https://statistics.gov.scot/data/deaths-involving-coronavirus-covid-19
#'
#' Full csv can be found here:
#' https://statistics.gov.scot/downloads/cube-table?uri=http%3A%2F%2Fstatistics.gov.scot%2Fdata%2Fdeaths-involving-coronavirus-covid-19
#'

library(SCRCdataAPI)

# Download source data
download_source_version(dataset = "scot_gov_deaths")

# Process data and generate hdf5 file
filename <- "data-raw/deaths-involving-coronavirus-covid-19.csv"
process_scot_gov_deaths(filename, h5filename)

