#' Data Zone Lookup Table
#'
#' Geography lookup tables used for aggregation, from 2011 data zones to higher
#' level geographies. (From: https://statistics.gov.scot/resource?uri=http%3A%2F%2Fstatistics.gov.scot%2Fdata%2Fdata-zone-lookup)
#'

library(SCRCdataAPI)

# Download source data
download_source_version(dataset = "scotgov_dz_lookup")

