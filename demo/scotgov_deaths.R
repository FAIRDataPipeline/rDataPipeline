#' This dataset presents the weekly, and year to date, provisional number of
#' deaths associated with coronavirus (COVID-19) alongside the total number
#' of deaths registered in Scotland, broken down by age, sex. (From: https://statistics.gov.scot/data/deaths-involving-coronavirus-covid-19)
#'

library(SCRCdataAPI)
library(SPARQL)

# Download source data
source_path <- file.path("human", "infection", "SARS-CoV-2", "scotland", "mortality")

post_source_data(
  storage_type = "ftp",
  storage_root = "Boydorr",
  storage_location = source_path,
  accessibility = "public",
  source_type = "database",
  source = "statistics.gov.scot",
  source_version_identifier = 1,
  source_version_description = "2020-06-23",
  responsible_person = "Sonia Mitchell",
  source_version_supercedes = "",
  key = read.table("token.txt"))

# Process data and generate hdf5 file
process_scot_gov_deaths(
  sourcefile = "data-raw/deaths-involving-coronavirus-covid-19.csv",
  h5filename = "deaths-involving-coronavirus-covid-19.h5")


script_path <- file.path("R", "process_scotgov_deaths.R")s
post_processing_script(storage_type = "GitHub",
                       storage_root = "SCRCdataAPI",
                       storage_location = script_path,
                       accessibility = "public",
                       processing_script = "process_scotgov_deaths.R",
                       script_version_identifier = 1,
                       responsible_person = "Sonia Mitchell",
                       script_version_supercedes = "",
                       key = read.table("token.txt"))

post_data_product()


