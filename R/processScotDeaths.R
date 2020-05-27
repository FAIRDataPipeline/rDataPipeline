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

  scotDeaths <- scotDeaths %>%
    dplyr::mutate(date = gsub("w/c ", "", date)) %>%
    dplyr::select(-type)

  covid_deaths_per_week <- scotDeaths %>%
    dplyr::filter(cause == "COVID-19 related",
                  date != "2020")

  # covid_deaths_per_week_nhsboard
  covid_deaths_per_week_nhsboard <- covid_deaths_per_week %>%
    dplyr::filter(areatypename == "Health Board Areas") %>%
    dplyr::select_if(~ length(unique(.)) != 1)

  # covid_deaths_per_week_councilarea
  covid_deaths_per_week_councilarea <- covid_deaths_per_week %>%
    dplyr::filter(areatypename == "Council Areas") %>%
    dplyr::select_if(~ length(unique(.)) != 1)

  covid_deaths_per_week_country <- covid_deaths_per_week %>%
    dplyr::filter(areatypename == "Country") %>%
    dplyr::select_if(~ length(unique(.)) != 1)

  # covid_deaths_per_week_agegroups
  covid_deaths_per_week_agegroups <- covid_deaths_per_week_country %>%
    dplyr::filter(age != "All") %>%
    dplyr::select_if(~ length(unique(.)) != 1)

  # covid_deaths_per_week_total
  covid_deaths_per_week_total <- covid_deaths_per_week_country %>%
    dplyr::filter(age == "All") %>%
    dplyr::select_if(~ length(unique(.)) != 1)

  assertthat::assert_that(nrow(covid_deaths_per_week) ==
                            (nrow(covid_deaths_per_week_nhsboard) +
                               nrow(covid_deaths_per_week_councilarea) +
                               nrow(covid_deaths_per_week_agegroups) +
                               nrow(covid_deaths_per_week_total)))
}











