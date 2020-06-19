#' coronavirus-covid-19-management-information
#'
#' This dataset presents Management Information, which is collected and
#' distributed each day in order to support understanding of the progress
#' of the outbreak in Scotland. (From: https://statistics.gov.scot/data/coronavirus-covid-19-management-information)
#'
#' Definitions found here:
#' https://www.gov.scot/publications/coronavirus-covid-19-data-definitions-and-sources/
#'

library(SCRCdataAPI)

# Download source data
download_source_version(dataset = "scotgov_management")

# Process data and generate hdf5 file
sourcefile <- "data-raw/coronavirus-covid-19-management-information.csv"
h5filename <- "coronavirus-covid-19-management-information.h5"
process_scotgov_management(sourcefile = sourcefile,
                           h5filename = h5filename)

openssl::sha256(file(h5filename))

