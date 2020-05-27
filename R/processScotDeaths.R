#' processScotDeaths
#'
#' deaths-involving-coronavirus-covid-19
#'
processScotDeaths <- function() {
  endpoint <- "https://statistics.gov.scot/sparql"

  query <- 'PREFIX qb: <http://purl.org/linked-data/cube#>
PREFIX data: <http://statistics.gov.scot/data/>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX dim: <http://purl.org/linked-data/sdmx/2009/dimension#>
PREFIX sdim: <http://statistics.gov.scot/def/dimension/>
PREFIX stat: <http://statistics.data.gov.uk/def/statistical-entity#>
PREFIX mp: <http://statistics.gov.scot/def/measure-properties/>
SELECT ?count ?type ?age ?cause ?location ?gender ?areaname ?areatypename ?date
WHERE {
  ?indicator qb:dataSet data:deaths-involving-coronavirus-covid-19 ;
              mp:count ?count ;
              qb:measureType ?measType ;
              sdim:age ?value ;
              sdim:causeOfDeath ?causeDeath ;
              sdim:locationOfDeath ?locDeath ;
              sdim:sex ?sex ;
              dim:refArea ?area ;
              dim:refPeriod ?period .

  ?measType rdfs:label ?type .
  ?value rdfs:label ?age .
  ?causeDeath rdfs:label ?cause .
  ?locDeath rdfs:label ?location .
  ?sex rdfs:label ?gender .
  ?area stat:code ?areatype ;
      rdfs:label ?areaname .
  ?areatype rdfs:label ?areatypename .
  ?period rdfs:label ?date .
}'

  scotDeaths <- SPARQL::SPARQL(endpoint, query)$results

  download.file("https://statistics.gov.scot/downloads/cube-table?uri=http%3A%2F%2Fstatistics.gov.scot%2Fdata%2Fdeaths-involving-coronavirus-covid-19",
                "deaths.csv")

  assertthat::assert_that(nrow(read.csv2("deaths.csv", sep = ",")) ==
                            nrow(scotDeaths))


  # Table1 - COVID deaths ---------------------------------------------------

  covid_deaths <- scotDeaths %>%
    dplyr::filter(cause == "COVID-19 related") %>%
    dplyr::select(-type)

  cd_week <- covid_deaths %>%
    dplyr::filter(date != "2020") %>%
    dplyr::mutate(date = gsub("^w/c", "", date))

  cd_total <- covid_deaths %>%
    dplyr::filter(date == "2020") %>%
    dplyr::select_if(~ length(unique(.)) != 1)

  # dataset 1 - covid_deaths_per_week_by_nhsboard
  covid_deaths_per_week_by_nhsboard <- cd_week %>%
    dplyr::filter(areatypename == "Health Board Areas") %>%
    dplyr::select_if(~ length(unique(.)) != 1)
  datasets <- c("covid_deaths_per_week_by_nhsboard")
  description <- c("Covid related deaths per week by NHS Health Board Area")

  # dataset (nhsboard - year to date)
  covid_deaths_by_nhsboard <- cd_total %>%
    dplyr::filter(areatypename == "Health Board Areas",
                  location == "All") %>%
    dplyr::select_if(~ length(unique(.)) != 1)

  # dataset 2 - covid_deaths_per_week_by_councilarea
  covid_deaths_per_week_by_councilarea <- cd_week %>%
    dplyr::filter(areatypename == "Council Areas") %>%
    dplyr::select_if(~ length(unique(.)) != 1)
  datasets <- c(datasets, "covid_deaths_per_week_by_councilarea")
  description <- c(description, "Covid related deaths per week by Council Area")

  # dataset (councilarea - year to date)
  covid_deaths_by_councilarea <- cd_total %>%
    dplyr::filter(areatypename == "Council Areas",
                  location == "All") %>%
    dplyr::select_if(~ length(unique(.)) != 1)

  cd_week_country <- cd_week %>%
    dplyr::filter(areatypename == "Country") %>%
    dplyr::select_if(~ length(unique(.)) != 1)

  # dataset 3 - covid_deaths_per_week_by_agegroup
  covid_deaths_per_week_by_agegroup <- cd_week_country %>%
    dplyr::filter(age != "All") %>%
    dplyr::select_if(~ length(unique(.)) != 1)
  datasets <- c(datasets, "covid_deaths_per_week_by_agegroup")
  description <- c(description, "Covid related deaths per week by age group")

  cd_week_allages <- cd_week_country %>%
    dplyr::filter(age == "All")

  # dataset 4 - covid_deaths_per_week_by_location
  covid_deaths_per_week_by_location <- cd_week_allages %>%
    dplyr::filter(location != "All") %>%
    dplyr::select_if(~ length(unique(.)) != 1)
  datasets <- c(datasets, "covid_deaths_per_week_by_location")
  description <- c(description, "Total Covid related deaths per week by location")

  # dataset 5 - covid_deaths_per_week
  covid_deaths_per_week <- cd_week_allages %>%
    dplyr::filter(location == "All")
  datasets <- c(datasets, "covid_deaths_per_week")
  description <- c(description, "Total Covid related deaths per week")

  # dataset (total & persons - year to date)
  covid_deaths_year_to_date <- cd_total %>%
    dplyr::filter(areatypename == "Country") %>%
    dplyr::select_if(~ length(unique(.)) != 1)

  assertthat::assert_that(nrow(cd_week) ==
                            (nrow(covid_deaths_per_week_by_nhsboard) +
                               nrow(covid_deaths_per_week_by_councilarea) +
                               nrow(covid_deaths_per_week_by_agegroup) +
                               nrow(covid_deaths_per_week_by_location) +
                               nrow(covid_deaths_per_week)))


  # Table2 - All deaths -----------------------------------------------------

  all_deaths <- scotDeaths %>%
    dplyr::filter(cause != "COVID-19 related") %>%
    dplyr::select_if(~ length(unique(.)) != 1)

  ad_week <- all_deaths %>%
    dplyr::filter(date != "2020") %>%
    dplyr::mutate(date = gsub("^w/c", "", date))

  ad_total <- all_deaths %>%
    dplyr::filter(date == "2020") %>%
    dplyr::select_if(~ length(unique(.)) != 1)

  # dataset 6 - all_deaths_per_week_by_nhsboard
  all_deaths_per_week_by_nhsboard <- ad_week %>%
    dplyr::filter(areatypename == "Health Board Areas") %>%
    dplyr::select_if(~ length(unique(.)) != 1)
  datasets <- c(datasets, "all_deaths_per_week_by_nhsboard")
  description <- c(description, "All deaths by NHS Health Board Area")

  # dataset (nhsboard - year to date)
  all_deaths_by_nhsboard <- ad_total %>%
    dplyr::filter(areatypename == "Health Board Areas",
                  location == "All") %>%
    dplyr::select_if(~ length(unique(.)) != 1)

  # dataset 7 - all_deaths_per_week_by_councilarea
  all_deaths_per_week_by_councilarea <- ad_week %>%
    dplyr::filter(areatypename == "Council Areas") %>%
    dplyr::select_if(~ length(unique(.)) != 1)
  datasets <- c(datasets, "all_deaths_per_week_by_councilarea")
  description <- c(description, "All deaths by Council Area")

  # dataset (councilarea - year to date)
  all_deaths_by_councilarea <- ad_total %>%
    dplyr::filter(areatypename == "Council Areas",
                  location == "All") %>%
    dplyr::select_if(~ length(unique(.)) != 1)

  ad_week_country <- ad_week %>%
    dplyr::filter(areatypename == "Country") %>%
    dplyr::select_if(~ length(unique(.)) != 1)

  # dataset 8 - all_deaths_per_week_by_agegroup
  all_deaths_per_week_by_agegroup <- ad_week_country %>%
    dplyr::filter(age != "All") %>%
    dplyr::select_if(~ length(unique(.)) != 1)
  datasets <- c(datasets, "all_deaths_per_week_by_agegroup")
  description <- c(description, "All deaths by age group")

  ad_week_allages <- ad_week_country %>%
    dplyr::filter(age == "All")

  # dataset 9 - all_deaths_per_week_by_location
  all_deaths_per_week_by_location <- ad_week_allages %>%
    dplyr::filter(location != "All") %>%
    dplyr::select_if(~ length(unique(.)) != 1)
  datasets <- c(datasets, "all_deaths_per_week_by_location")
  description <- c(description, "Total deaths per week by location")

  # dataset 10 - all_deaths_per_week
  all_deaths_per_week <- ad_week_allages %>%
    dplyr::filter(location == "All")
  datasets <- c(datasets, "all_deaths_per_week")
  description <- c(description, "Total deaths per week")

  # dataset (total & persons - year to date)
  all_deaths_year_to_date <- ad_total %>%
    dplyr::filter(areatypename == "Country") %>%
    dplyr::select_if(~ length(unique(.)) != 1)

  assertthat::assert_that(nrow(ad_week) ==
                            (nrow(all_deaths_per_week_by_nhsboard) +
                               nrow(all_deaths_per_week_by_councilarea) +
                               nrow(all_deaths_per_week_by_agegroup) +
                               nrow(all_deaths_per_week_by_location) +
                               nrow(all_deaths_per_week)))


  # Table3 - deaths by location ---------------------------------------------

  # dataset 11 - covid_deaths_by_nhsboard_and_location
  covid_deaths_by_nhsboard_and_location <- cd_total %>%
    dplyr::filter(areatypename == "Health Board Areas",
                  location != "All") %>%
    dplyr::select_if(~ length(unique(.)) != 1)
  datasets <- c(datasets, "covid_deaths_by_nhsboard_and_location")
  description <- c(description, paste0(
    "Covid related deaths by NHS Health Board Area and location"))

  # dataset 12 - all_deaths_by_nhsboard_and_location
  all_deaths_by_nhsboard_and_location <- ad_total %>%
    dplyr::filter(areatypename == "Health Board Areas",
                  location != "All") %>%
    dplyr::select_if(~ length(unique(.)) != 1)
  datasets <- c(datasets, "all_deaths_by_nhsboard_and_location")
  description <- c(description, paste0(
    "All deaths by NHS Health Board Area and location"))

  # dataset 13 - covid_deaths_by_councilarea_and_location
  covid_deaths_by_councilarea_and_location <- cd_total %>%
    dplyr::filter(areatypename == "Council Areas",
                  location != "All") %>%
    dplyr::select_if(~ length(unique(.)) != 1)
  datasets <- c(datasets, "covid_deaths_by_councilarea_and_location")
  description <- c(description, paste0(
    "Covid related deaths by Council Area and location"))

  # dataset 14 - all_deaths_by_councilarea_and_location
  all_deaths_by_councilarea_and_location <- ad_total %>%
    dplyr::filter(areatypename == "Council Areas",
                  location != "All") %>%
    dplyr::select_if(~ length(unique(.)) != 1)
  datasets <- c(datasets, "all_deaths_by_councilarea_and_location")
  description <- c(description, paste0(
    "All deaths by Council Area and location"))

  # -------------------------------------------------------------------------


  # Create hdf5 file
  h5filename <- "deaths-involving-coronavirus-covid-19.h5"
  rhdf5::h5createFile(h5filename)
  rhdf5::h5createGroup(h5filename, "datasets")

  # Attach datasets
  for (i in seq_along(datasets))
    rhdf5::h5write(get(datasets[i]), file = h5filename,
                   name = file.path("datasets/", datasets[i]))

  # Attach attributes to group
  fid <- rhdf5::H5Fopen(h5filename)
  gid <- rhdf5::H5Gopen(fid, name = "datasets/")

  rhdf5::h5writeAttribute(gid, attr = paste0(
    "This dataset presents the weekly, and year to date, provisional number",
    "of deaths associated with coronavirus (COVID-19) alongside the total",
    "number of deaths registered in Scotland, broken down by age, sex."),
    name = "Description")
  rhdf5::h5writeAttribute(gid, attr = Sys.time(), name = "DownloadDate")
  rhdf5::h5writeAttribute(gid, attr = "National Records of Scotland",
                          name = "Source")
  rhdf5::h5writeAttribute(gid, attr = paste0(
    "https://statistics.gov.scot/resource?uri=http%3A%2F%2Fstatistics.gov.",
    "scot%2Fdata%2Fdeaths-involving-coronavirus-covid-19"), name = "URL")

  # Attach attributes to datasets
  for(i in seq_along(datasets)) {
    did <- rhdf5::H5Dopen(fid, paste0("datasets/", datasets[i]))
    rhdf5::h5writeAttribute(did, attr = description[i],
                            name = "Description")
    rhdf5::H5Dclose(did)
  }

  rhdf5::H5Gclose(gid)
  rhdf5::H5Fclose(fid)

}











