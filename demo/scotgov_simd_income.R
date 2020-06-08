#' The Scottish Index of Multiple Deprivation (SIMD) provides a relative
#' ranking of the data zones in Scotland from 1 (most deprived) to 6,976
#' (least deprived) based on a weighted combination of data in the domains
#' of Income; Employment; Health; Education, Skills and Training; Geographic
#' Access to Services; Crime; and Housing. (From: https://statistics.gov.scot/resource?uri=http%3A%2F%2Fstatistics.gov.scot%2Fdata%2Fscottish-index-of-multiple-deprivation)
#' Only income has been included in this dataset.
#'

library(SCRCdataAPI)
library(SPARQL)

# Download source data
download_source_version(dataset = "scotgov_simd_income")

