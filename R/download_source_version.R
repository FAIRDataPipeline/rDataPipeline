#' download_source_version
#'
#' @param dataset name of dataset c("scot_dz_shapefile", "scot_dz_demographics",
#' "scot_gov_deaths", "scot_gov_simd")
#'
#' @export
#'
download_source_version <- function(dataset) {

  require(SPARQL, quietly = TRUE)

  if(dataset == "ukgov_scot_dz_shapefile") {

    # ukgov_scot_dz_shapefile -------------------------------------------------

    download_from_url(
      url = "http://sedsh127.sedsh.gov.uk",
      path = file.path("Atom_data", "ScotGov", "ZippedShapefiles",
                       "SG_DataZoneBdry_2011.zip"),
      local = file.path("data-raw", "datazone_shapefile"))

  } else if(dataset == "scotgov_dz_lookup") {

    # scotgov_dz_lookup -----------------------------------------------------

    download_from_url(
      url = "http://statistics.gov.scot",
      path = file.path(
        "downloads",
        "file?id=5a9bf61e-7571-45e8-a307-7c1218d5f6b5%2FDatazone2011Lookup.csv"),
      local = "data-raw",
      filename = "scotgov_dz_lookup.csv")
    download_from_url(
      url = "https://www.gov.scot",
      path = file.path(
        "binaries",
        "content",
        "documents",
        "govscot",
        "publications",
        "statistics",
        "2020",
        "01",
        "scottish-index-of-multiple-deprivation-2020-data-zone-look-up-file",
        "documents",
        "scottish-index-of-multiple-deprivation-data-zone-look-up",
        "scottish-index-of-multiple-deprivation-data-zone-look-up",
        "govscot%3Adocument",
        "SIMD%2B2020v2%2B-%2Bdatazone%2Blookup.xlsx?forceDownload=true"),
      local = "data-raw",
      filename = "scotgov_simd_lookup.xlsx")

  } else if(dataset == "ons_demographics") {

    # ons_eng_wales_population ----------------------------------------------
    
    genders <- c("Persons", "Males", "Females")
    for (sex in seq_along(genders)) {
      for (age in 101:191) {
        for (step in c(0,((c(24000)*c(1:7))))){
          download_from_url(url="https://www.nomisweb.co.uk"
                            , path=sprintf("api/v01/dataset/NM_2010_1.data.csv?measures=20100&time=latest&geography=TYPE299&gender=%d&c_age=%d&RecordLimit=24000&RecordOffset=%d", sex-1,age, step ), 
                            local="data-raw", filename=sprintf("populationstore_g%d_a%d_s%d.csv", sex-1,age, step ))
          temp_pop_table <-read.csv(sprintf("data-raw/populationstore_g%d_a%d_s%d.csv", sex-1,age, step )) %>%
            dplyr::select(DATE, GEOGRAPHY_NAME, GEOGRAPHY_CODE, 
                          GEOGRAPHY_TYPE, GENDER_NAME, C_AGE_NAME, 
                          MEASURES_NAME, OBS_VALUE)
          file.remove(sprintf("data-raw/populationstore_g%d_a%d_s%d.csv", sex-1,age, step ))
          names(temp_pop_table)[8] <- unique(temp_pop_table$C_AGE_NAME)
          geography_value <- temp_pop_table[, c(2, 8)]
          
          if(step==0){
            temp_population_table=geography_value
          }else{
            temp_population_table=rbind(temp_population_table,geography_value)
          }
        }
        if (age == 101) {
          population_table = temp_population_table
        }
        else {
          population_table = left_join(population_table, 
                                       geography_value, by = "GEOGRAPHY_NAME")
        }
      }
      write.csv(population_table, paste0("data-raw/england_", genders[sex], 
                                         ".csv"),row.names = F)
    }

  } else if(dataset == "nrs_demographics") {

    # nrs_demographics ------------------------------------------------------

    download_from_url(
      url = "https://www.nrscotland.gov.uk",
      path = file.path("files//statistics", "population-estimates",
                       "sape-time-series", "males", "sape-2018-males.xlsx"),
      local = "data-raw")

    download_from_url(
      url = "https://www.nrscotland.gov.uk",
      path = file.path("files//statistics", "population-estimates",
                       "sape-time-series/females/sape-2018-females.xlsx"),
      local = "data-raw")

    download_from_url(
      url = "https://www.nrscotland.gov.uk",
      path = file.path("files//statistics", "population-estimates",
                       "sape-time-series", "persons", "sape-2018-persons.xlsx"),
      local = "data-raw")

  } else if(dataset == "scotgov_deaths") {

    # deaths-involving-coronavirus-covid-19 -----------------------------------

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
              sdim:causeofdeath ?causeDeath;
              sdim:locationofdeath ?locDeath;
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

  } else if(dataset == "scotgov_simd_income") {

    # scottish-index-of-multiple-deprivation ----------------------------------

    # Rank
    query <- "PREFIX qb: <http://purl.org/linked-data/cube#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX data: <http://statistics.gov.scot/data/>
PREFIX sdmxd: <http://purl.org/linked-data/sdmx/2009/dimension#>
PREFIX dim: <http://statistics.gov.scot/def/dimension/>
PREFIX dom: <http://statistics.gov.scot/def/concept/simd-domain/>
PREFIX ref: <http://reference.data.gov.uk/id/year/>
PREFIX stat: <http://statistics.data.gov.uk/def/statistical-entity#>
PREFIX mp: <http://statistics.gov.scot/def/measure-properties/>
SELECT ?featurecode ?featurename ?areatypename ?date ?measuretype ?values
WHERE {
  ?indicator qb:dataSet data:scottish-index-of-multiple-deprivation;
              mp:rank ?values;
              qb:measureType ?measure;
              dim:simdDomain dom:income;
              sdmxd:refArea ?featurecode;
              sdmxd:refPeriod ?period.

              ?featurecode stat:code ?areatype;
                rdfs:label ?featurename.
              ?areatype rdfs:label ?areatypename.
              ?period rdfs:label ?date.
              ?measure rdfs:label ?measuretype.
}"

    fn <- "scottish-index-of-multiple-deprivation-income-rank.csv"
    download_from_db(url = "https://statistics.gov.scot/sparql",
                     path = query,
                     local = "data-raw",
                     filename = fn)

    # Quintile
    query <- "PREFIX qb: <http://purl.org/linked-data/cube#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX data: <http://statistics.gov.scot/data/>
PREFIX sdmxd: <http://purl.org/linked-data/sdmx/2009/dimension#>
PREFIX dim: <http://statistics.gov.scot/def/dimension/>
PREFIX dom: <http://statistics.gov.scot/def/concept/simd-domain/>
PREFIX ref: <http://reference.data.gov.uk/id/year/>
PREFIX stat: <http://statistics.data.gov.uk/def/statistical-entity#>
PREFIX mp: <http://statistics.gov.scot/def/measure-properties/>
SELECT ?featurecode ?featurename ?areatypename ?date ?measuretype ?values
WHERE {
  ?indicator qb:dataSet data:scottish-index-of-multiple-deprivation;
              mp:quintile ?values;
              qb:measureType ?measure;
              dim:simdDomain dom:income;
              sdmxd:refArea ?featurecode;
              sdmxd:refPeriod ?period.

              ?featurecode stat:code ?areatype;
                rdfs:label ?featurename.
              ?areatype rdfs:label ?areatypename.
              ?period rdfs:label ?date.
              ?measure rdfs:label ?measuretype.

}"

    fn <- "scottish-index-of-multiple-deprivation-income-quintile.csv"
    download_from_db(url = "https://statistics.gov.scot/sparql",
                     path = query,
                     local = "data-raw",
                     filename = fn)

    # Decile
    query <- "PREFIX qb: <http://purl.org/linked-data/cube#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX data: <http://statistics.gov.scot/data/>
PREFIX sdmxd: <http://purl.org/linked-data/sdmx/2009/dimension#>
PREFIX dim: <http://statistics.gov.scot/def/dimension/>
PREFIX dom: <http://statistics.gov.scot/def/concept/simd-domain/>
PREFIX ref: <http://reference.data.gov.uk/id/year/>
PREFIX stat: <http://statistics.data.gov.uk/def/statistical-entity#>
PREFIX mp: <http://statistics.gov.scot/def/measure-properties/>
SELECT ?featurecode ?featurename ?areatypename ?date ?measuretype ?values
WHERE {
  ?indicator qb:dataSet data:scottish-index-of-multiple-deprivation;
              mp:decile ?values;
              qb:measureType ?measure;
              dim:simdDomain dom:income;
              sdmxd:refArea ?featurecode;
              sdmxd:refPeriod ?period.

              ?featurecode stat:code ?areatype;
                rdfs:label ?featurename.
              ?areatype rdfs:label ?areatypename.
              ?period rdfs:label ?date.
              ?measure rdfs:label ?measuretype.
}"

    fn <- "scottish-index-of-multiple-deprivation-income-decile.csv"
    download_from_db(url = "https://statistics.gov.scot/sparql",
                     path = query,
                     local = "data-raw",
                     filename = fn)

    # Vigintile
    query <- "PREFIX qb: <http://purl.org/linked-data/cube#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX data: <http://statistics.gov.scot/data/>
PREFIX sdmxd: <http://purl.org/linked-data/sdmx/2009/dimension#>
PREFIX dim: <http://statistics.gov.scot/def/dimension/>
PREFIX dom: <http://statistics.gov.scot/def/concept/simd-domain/>
PREFIX ref: <http://reference.data.gov.uk/id/year/>
PREFIX stat: <http://statistics.data.gov.uk/def/statistical-entity#>
PREFIX mp: <http://statistics.gov.scot/def/measure-properties/>
SELECT ?featurecode ?featurename ?areatypename ?date ?measuretype ?values
WHERE {
  ?indicator qb:dataSet data:scottish-index-of-multiple-deprivation;
              mp:vigintile ?values;
              qb:measureType ?measure;
              dim:simdDomain dom:income;
              sdmxd:refArea ?featurecode;
              sdmxd:refPeriod ?period.

              ?featurecode stat:code ?areatype;
                rdfs:label ?featurename.
              ?areatype rdfs:label ?areatypename.
              ?period rdfs:label ?date.
              ?measure rdfs:label ?measuretype.
}"

    fn <- "scottish-index-of-multiple-deprivation-income-vigintile.csv"
    download_from_db(url = "https://statistics.gov.scot/sparql",
                     path = query,
                     local = "data-raw",
                     filename = fn)

  } else if(dataset == "scotgov_ur_classification") {

    # urban-rural-classification ----------------------------------------------

    query <- "PREFIX qb: <http://purl.org/linked-data/cube#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX data: <http://statistics.gov.scot/data/>
PREFIX sdmxd: <http://purl.org/linked-data/sdmx/2009/dimension#>
PREFIX stat: <http://statistics.data.gov.uk/def/statistical-entity#>
PREFIX mp: <http://statistics.gov.scot/def/measure-properties/>
PREFIX year:<http://reference.data.gov.uk/id/year/>
SELECT ?featurecode ?featurename ?areatypename ?rank
WHERE {
  ?indicator qb:dataSet data:urban-rural-classification;
              mp:rank ?rank;
              sdmxd:refArea ?featurecode;
              sdmxd:refPeriod year:2016.

              ?featurecode stat:code ?areatype;
                rdfs:label ?featurename.
              ?areatype rdfs:label ?areatypename.
}"

    download_from_db(url = "https://statistics.gov.scot/sparql",
                     path = query,
                     local = "data-raw",
                     filename = "urban-rural-classification.csv")

  } else if(dataset == "scotgov_management") {

    # coronavirus-covid-19-management-information -----------------------------

    query <- "PREFIX qb: <http://purl.org/linked-data/cube#>
PREFIX data: <http://statistics.gov.scot/data/>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX mp: <http://statistics.gov.scot/def/measure-properties/>
PREFIX dim: <http://purl.org/linked-data/sdmx/2009/dimension#>
PREFIX sdim: <http://statistics.gov.scot/def/dimension/>
PREFIX stat: <http://statistics.data.gov.uk/def/statistical-entity#>
SELECT ?featurecode ?featurename ?date ?measure ?variable ?count
WHERE {
  ?indicator qb:dataSet data:coronavirus-covid-19-management-information;
              dim:refArea ?featurecode;
              dim:refPeriod ?period;
              sdim:variable ?varname;
              qb:measureType ?type.
{?indicator mp:count ?count.} UNION {?indicator mp:ratio ?count.}

  ?featurecode <http://publishmydata.com/def/ontology/foi/displayName> ?featurename.
  ?period rdfs:label ?date.
  ?varname rdfs:label ?variable.
  ?type rdfs:label ?measure.
}"

    download_from_db(url = "https://statistics.gov.scot/sparql",
                     path = query,
                     local = "data-raw",
                     filename =
                       "coronavirus-covid-19-management-information.csv")

  } else if(dataset == "ukgov_eng_oa_shapefile") {

    download_from_url(url = "https://opendata.arcgis.com/datasets",
                      path = "ff8151d927974f349de240e7c8f6c140_0.zip?outSR=%7B%22latestWkid%22%3A3857%2C%22wkid%22%3A102100%7D",
                      local = "data-raw/outputarea_shapefile",
                      filename = "ff8151d927974f349de240e7c8f6c140_0.zip")

  } else if(dataset == "ukgov_eng_lookup") {

    download_from_url(url = "http://geoportal1-ons.opendata.arcgis.com/datasets",
                      path = "c721b6da8ea04f189baa27a1f3e32e06_0.csv",
                      local = "data-raw/england_lookup",
                      filename = "output_to_ward_to_LA.csv")

    download_from_url(url = "http://geoportal1-ons.opendata.arcgis.com/datasets",
                      path = "6ecda95a83304543bc8feedbd1a58303_0.csv",
                      local = "data-raw/england_lookup",
                      filename = "output_to_LSOA_MSOA_to_LA.csv")

    download_from_url(url = "https://opendata.arcgis.com/datasets",
                      path = "520e9cd294c84dfaaf97cc91494237ac_0.csv",
                      local = "data-raw/england_lookup",
                      filename = "LSOA_to_CCG.csv")

    download_from_url(url = "http://geoportal1-ons.opendata.arcgis.com/datasets",
                      path = "e6d0a1c8ce3344a7b79ce1c24e3174c9_0.csv",
                      local = "data-raw/england_lookup",
                      filename = "ward_to_UA_wales.csv")

    download_from_url(url = "https://opendata.arcgis.com/datasets",
                      path = "680c9b730655473787cb594f328a86fa_0.csv",
                      local = "data-raw/england_lookup",
                      filename = "UA_to_healthboard_wales.csv")

  } else
    stop("Dataset not recognised.")

}

