#' process_scotgov_management
#'
#' @export
#'
process_scotgov_management <- function() {
scotMan <- SPARQL::SPARQL(endpoint, query)$results

# download.file("https://statistics.gov.scot/downloads/cube-table?uri=http%3A%2F%2Fstatistics.gov.scot%2Fdata%2Fcoronavirus-covid-19-management-information",
#               "counts.csv")
#
# assertthat::assert_that(nrow(read.csv2("counts.csv", sep = ",")) ==
#                           nrow(scotMan))

scotMan <- scotMan %>%
  mutate(featurecode =
           gsub("<http://statistics.gov.scot/id/statistical-geography/", "",
                featurecode),
         featurecode = gsub(">", "", featurecode))



# 1 -----------------------------------------------------------------------
# Numbers of calls to NHS 111 and the coronavirus helpline
nhs24.dat <- scotMan %>% dplyr::filter(grepl("Calls", variable))
datasets <- c("nhs24.dat")

# 2 -----------------------------------------------------------------------
# Numbers of people in hospital and in ICU with confirmed or suspected COVID-19
hospital.dat <- scotMan %>% dplyr::filter(grepl("COVID-19 patients", variable))
datasets <- c(datasets, "hospital.dat")

# part1 <- cbind.data.frame(hospital_care[, 1:4], Location = "ICU")
# colnames(part1) <- gsub("\\...[0-9]$", "", colnames(part1))
# part2 <- cbind.data.frame(hospital_care[, c(1,5:7)], location = "hospital")
# colnames(part2) <- gsub("\\...[0-9]$", "", colnames(part1))
# hospital_care <- rbind.data.frame(part1, part2)
# colnames(hospital_care)[1] <- "Date"


# 3 -----------------------------------------------------------------------
# Numbers of ambulance attendances (total and COVID-19 suspected) and number of
# people taken to hospital with suspected COVID-19
ambulance.dat <- scotMan %>% dplyr::filter(grepl("Ambulance attendances", variable))
datasets <- c(datasets, "ambulance.dat")



# 4 -----------------------------------------------------------------------
# Number of people delayed in hospital
discharges.dat <- scotMan %>% dplyr::filter(grepl("Delayed discharges", variable))
datasets <- c(datasets, "discharges.dat")




# 5 -----------------------------------------------------------------------
# Numbers of people tested to date, numbers with positive and negative results,
# and numbers of tests carried out
testing.dat <- scotMan %>% dplyr::filter(grepl("Testing", variable))
datasets <- c(datasets, "testing.dat")



# headers <- testing[1,] %>% unlist()
# headers[1] <- colnames(testing)[1]
# headers[5:6] <- paste0("nhs_", headers[5:6])
# headers[7:8] <- paste0("regional_", headers[7:8])
# colnames(testing) <- tolower(headers)
# testing <- testing[-1,]


# 6 -----------------------------------------------------------------------
# Numbers of NHS workforce reporting as absent due to a range of reasons
# related to Covid-19
nhs.dat <- scotMan %>% dplyr::filter(grepl("NHS workforce", variable))
datasets <- c(datasets, "nhs.dat")



# 7 -----------------------------------------------------------------------
# Number of care homes where suspected COVID-19 has been reported to date
carehomes.dat <- scotMan %>% dplyr::filter(grepl("Adult care homes", variable))
datasets <- c(datasets, "carehomes.dat")


# 8 -----------------------------------------------------------------------
# Number of COVID-19 confirmed deaths registered to date
deaths.dat <- scotMan %>% dplyr::filter(grepl("deaths registered", variable))
datasets <- c(datasets, "deaths.dat")




assertthat::assert_that(nrow(scotMan) ==
                          sum(sapply(datasets, function(x) nrow(get(x)))))


}











