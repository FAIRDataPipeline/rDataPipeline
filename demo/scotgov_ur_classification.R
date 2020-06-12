#' The Scottish Government Urban Rural Classification provides a consistent
#' way of defining urban and rural areas across Scotland. The classification
#' is based upon two main criteria: (i) population as defined by the National
#' Records of Scotland, and (ii) accessibility based on drive time analysis
#' to differentiate between accessible and remote areas in Scotland.
#' The classification distinguishes between urban, rural and remote areas
#' within Scotland and includes the following categories:
#'   1 - Large Urban Areas Settlements of over 125,000 people;
#' 2 - Other Urban Areas Settlements of 10,000 to 125,000 people;
#' 3 - Accessible Small Towns Settlements of between 3,000 and 10,000 people
#' and within 30 minutes drive of a settlement of 10,000 or more;
#' 4 - Remote Small Towns Settlements of between 3,000 and 10,000 people and
#' with a drive time of over 30 minutes to a settlement of 10,000 or more;
#' 5 - Accessible Rural Settlements of less than 3,000 people and within 30
#' minutes drive of a settlement of 10,000 or more;
#' 6 - Remote Rural Settlements of less than 3,000 people and with a drive
#' time of over 30 minutes to a settlement of 10,000 or more.
#' (From: https://statistics.gov.scot/resource?uri=http%3A%2F%2Fstatistics.gov.scot%2Fdata%2Furban-rural-classification)
#'

library(SCRCdataAPI)
library(SPARQL)

# Download source data
download_source_version(dataset = "scotgov_ur_classification")

# Process data and generate hdf5 file
sourcefile <- "data-raw/urban-rural-classification.csv"
h5filename <- "urban-rural-classification.h5"
process_scotgov_ur_classification(sourcefile = sourcefile,
                                  h5filename = h5filename)

openssl::sha256(file(h5filename))


