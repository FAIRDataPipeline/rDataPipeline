#' Small Area Population Estimates 2018. Estimated population by sex,
#' single year of age, 2011 Data Zone area, and council area: 30 June 2018.
#' (From: https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/population/population-estimates/2011-based-special-area-population-estimates/small-area-population-estimates/time-series)
#'

library(SCRCdataAPI)

# Download source data
download_source_version(dataset = "ons_demographics")

# Process data and generate hdf5 file
# sourcefile <- c("data-raw/sape-2018-persons.xlsx",
#                 "data-raw/sape-2018-females.xlsx",
#                 "data-raw/sape-2018-males.xlsx")

h5filename <- "england_population.h5"

output_area_sf <- file.path("data-raw", "outputarea_shapefile",
                         "Output_Areas__December_2011__Boundaries_EW_BFC.shp")

oa_conversion_table <- file.path("data-raw", "oa_conversion_table.csv")
grp.names <- c("dz", "ur", "iz", "la", "hb", "mmw", "spc", "grid1km",
               "grid10km")
full.names <- c("data zone", "urban rural classification",
                "intermediate zone", "local authority",
                "health board", "multi member ward",
                "scottish parliamentary constituency", "grid area",
                "grid area")
subgrp.names <- c("total", "1year", "5year", "10year",
                  "sg_deaths_scheme")
age.classes <- list("total", 0:90, seq(0, 90, 5), seq(0, 90, 10),
                    c(0, 1, 15, 45, 65, 75, 85))

process_ons_demographics(sourcefile = sourcefile,
                         h5filename = h5filename,
                         output_area_sf,
                         oa_conversion_table,
                         grp.names,
                         full.names,
                         subgrp.names,
                         age.classes)

openssl::sha256(file(h5filename))
