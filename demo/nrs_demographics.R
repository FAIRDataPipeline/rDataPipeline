#' Small Area Population Estimates 2018. Estimated population by sex,
#' single year of age, 2011 Data Zone area, and council area: 30 June 2018.
#' (From: https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/population/population-estimates/2011-based-special-area-population-estimates/small-area-population-estimates/time-series)
#'

library(SCRCdataAPI)
library(SPARQL)

# Download source data
download_source_version(dataset = "nrs_demographics")

# Process data and generate hdf5 file
sourcefile <- c("data-raw/sape-2018-persons.xlsx",
                "data-raw/sape-2018-females.xlsx",
                "data-raw/sape-2018-males.xlsx")
h5filename <- "demographics.h5"
process_nrs_demographics(sourcefile = sourcefile,
                         h5filename = h5filename)

openssl::sha256(file("demographics.h5"))
