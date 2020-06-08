#' download_source_version
#'
#' @param dataset name of dataset c("scot_dz_shapefile", "scot_dz_demographics",
#' "scot_gov_deaths", "scot_gov_simd")
#'
#' @export
#'
download_source_version <- function(dataset) {

  if(dataset == "ukgov_scot_dz_shapefile") {
    download_from_url(
      url = "http://sedsh127.sedsh.gov.uk",
      path = "Atom_data/ScotGov/ZippedShapefiles/SG_DataZoneBdry_2011.zip",
      local = "data-raw/datazone_shapefile")

  } else if(dataset == "scotgov_dz_lookup") {
    download_from_url(
      url = "http://statistics.gov.scot",
      path = "downloads/file?id=5a9bf61e-7571-45e8-a307-7c1218d5f6b5%2FDatazone2011Lookup.csv",
      local = "data-raw",
      filename = "scotgov_dz_lookup.csv")

  } else if(dataset == "nrs_demographics") {
    download_from_url(
      url = "https://www.nrscotland.gov.uk",
      path = file.path("files//statistics/population-estimates/sape-time-series/males/sape-2018-males.xlsx"),
      local = "data-raw")

    download_from_url(
      url = "https://www.nrscotland.gov.uk",
      path = file.path("files//statistics/population-estimates/sape-time-series/females/sape-2018-females.xlsx"),
      local = "data-raw")

    download_from_url(
      url = "https://www.nrscotland.gov.uk",
      path = file.path("files//statistics/population-estimates/sape-time-series/persons/sape-2018-persons.xlsx"),
      local = "data-raw")

  } else if(dataset == "scotgov_deaths") {
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
                     filename = "scotgov_deaths.csv")

  }else if(dataset == "scotgov_simd_income") {
    query <- "PREFIX qb: <http://purl.org/linked-data/cube#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX data: <http://statistics.gov.scot/data/>
PREFIX sdmxd: <http://purl.org/linked-data/sdmx/2009/dimension#>
PREFIX dim: <http://statistics.gov.scot/def/dimension/>
PREFIX dom: <http://statistics.gov.scot/def/concept/simd-domain/>
PREFIX ref: <http://reference.data.gov.uk/id/year/>
PREFIX stat: <http://statistics.data.gov.uk/def/statistical-entity#>
PREFIX mp: <http://statistics.gov.scot/def/measure-properties/>
SELECT ?featurecode ?featurename ?areatypename ?date ?rank
WHERE {
  ?indicator qb:dataSet data:scottish-index-of-multiple-deprivation;
              mp:rank ?rank;
              dim:simdDomain dom:income;
              sdmxd:refArea ?featurecode;
              sdmxd:refPeriod ?period.

              ?featurecode stat:code ?areatype;
                rdfs:label ?featurename.
              ?areatype rdfs:label ?areatypename.
              ?period rdfs:label ?date.
}"

    download_from_db(url = "https://statistics.gov.scot/sparql",
                     path = query,
                     local = "data-raw",
                     filename = "scotgov_simd_income.csv")
  }

}

