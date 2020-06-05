#' download_source_version
#'
#' @param dataset name of dataset
#'
#' @export
#'
download_source_version <- function(dataset) {

  if(dataset == "scot_dz_shapefile") {
    download_from_url(
      url = "http://sedsh127.sedsh.gov.uk",
      path = "Atom_data/ScotGov/ZippedShapefiles/SG_DataZoneBdry_2011.zip",
      local = "data-raw/datazone_shapefile")

  } else if(dataset == "scot_dz_demographics") {
    download_from_url(
      url = "https://www.nrscotland.gov.uk",
      path = file.path("files//statistics/population-estimates/sape-time-series/males/sape-2018-males.xlsx"),
      local = "data-raw")

    download_from_url(
      url = "https://www.nrscotland.gov.uk",
      path = file.path("files//statistics/population-estimates/sape-time-series/females/sape-2018-females.xlsx"),
      local = "data-raw")

  }else if(dataset == "scot_gov_deaths") {
    query <- "PREFIX qb: <http://purl.org/linked-data/cube#>
PREFIX data: <http://statistics.gov.scot/data/>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX dim: <http://purl.org/linked-data/sdmx/2009/dimension#>
PREFIX sdim: <http://statistics.gov.scot/def/dimension/>
PREFIX stat: <http://statistics.data.gov.uk/def/statistical-entity#>
PREFIX mp: <http://statistics.gov.scot/def/measure-properties/>
SELECT ?featurecode ?featurename ?areatypename ?date ?cause ?location ?gender ?age ?type ?count
WHERE {
  ?indicator qb:dataSet data:deaths-involving-coronavirus-covid-19;
              mp:count ?count;
              qb:measureType ?measType;
              sdim:age ?value;
              sdim:causeOfDeath ?causeDeath;
              sdim:locationOfDeath ?locDeath;
              sdim:sex ?sex;
              dim:refArea ?featurecode;
              dim:refPeriod ?period.

              ?measType rdfs:label ?type.
              ?value rdfs:label ?age.
              ?causeDeath rdfs:label ?cause.
              ?locDeath rdfs:label ?location.
              ?sex rdfs:label ?gender.
              ?featurecode stat:code ?areatype;
                rdfs:label ?featurename.
              ?areatype rdfs:label ?areatypename.
              ?period rdfs:label ?date.
}"
    download_from_db(url = "https://statistics.gov.scot/sparql",
                     path = query,
                     local = "data-raw",
                     filename = "deaths-involving-coronavirus-covid-19.csv")

  }

}

