download_source_version<-function (dataset) 
{
  if (dataset == "ukgov_scot_dz_shapefile") {
    download_from_url(url = "http://sedsh127.sedsh.gov.uk", 
                      path = "Atom_data/ScotGov/ZippedShapefiles/SG_DataZoneBdry_2011.zip", 
                      local = "data-raw/datazone_shapefile")
  }
  else if (dataset == "scotgov_dz_lookup") {
    download_from_url(url = "http://statistics.gov.scot", 
                      path = "downloads/file?id=5a9bf61e-7571-45e8-a307-7c1218d5f6b5%2FDatazone2011Lookup.csv", 
                      local = "data-raw", filename = "scotgov_dz_lookup.csv")
  }
  else if (dataset == "nrs_demographics") {
    download_from_url(url = "https://www.nrscotland.gov.uk", 
                      path = file.path("files//statistics/population-estimates/sape-time-series/males/sape-2018-males.xlsx"), 
                      local = "data-raw")
    download_from_url(url = "https://www.nrscotland.gov.uk", 
                      path = file.path("files//statistics/population-estimates/sape-time-series/females/sape-2018-females.xlsx"), 
                      local = "data-raw")
    download_from_url(url = "https://www.nrscotland.gov.uk", 
                      path = file.path("files//statistics/population-estimates/sape-time-series/persons/sape-2018-persons.xlsx"), 
                      local = "data-raw")
  }
  else if (dataset == "scotgov_deaths") {
    query <- "PREFIX qb: <http://purl.org/linked-data/cube#>\nPREFIX data: <http://statistics.gov.scot/data/>\nPREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>\nPREFIX dim: <http://purl.org/linked-data/sdmx/2009/dimension#>\nPREFIX sdim: <http://statistics.gov.scot/def/dimension/>\nPREFIX stat: <http://statistics.data.gov.uk/def/statistical-entity#>\nPREFIX mp: <http://statistics.gov.scot/def/measure-properties/>\nSELECT ?featurecode ?featurename ?areatypename ?date ?cause ?location ?gender ?age ?type ?count\nWHERE {\n  ?indicator qb:dataSet data:deaths-involving-coronavirus-covid-19;\n              mp:count ?count;\n              qb:measureType ?measType;\n              sdim:age ?value;\n              sdim:causeOfDeath ?causeDeath;\n              sdim:locationOfDeath ?locDeath;\n              sdim:sex ?sex;\n              dim:refArea ?featurecode;\n              dim:refPeriod ?period.\n\n              ?measType rdfs:label ?type.\n              ?value rdfs:label ?age.\n              ?causeDeath rdfs:label ?cause.\n              ?locDeath rdfs:label ?location.\n              ?sex rdfs:label ?gender.\n              ?featurecode stat:code ?areatype;\n                rdfs:label ?featurename.\n              ?areatype rdfs:label ?areatypename.\n              ?period rdfs:label ?date.\n}"
    download_from_db(url = "https://statistics.gov.scot/sparql", 
                     path = query, local = "data-raw", filename = "scotgov_deaths.csv")
  }
  else if (dataset == "scotgov_simd_income") {
    query <- "PREFIX qb: <http://purl.org/linked-data/cube#>\nPREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>\nPREFIX data: <http://statistics.gov.scot/data/>\nPREFIX sdmxd: <http://purl.org/linked-data/sdmx/2009/dimension#>\nPREFIX dim: <http://statistics.gov.scot/def/dimension/>\nPREFIX dom: <http://statistics.gov.scot/def/concept/simd-domain/>\nPREFIX ref: <http://reference.data.gov.uk/id/year/>\nPREFIX stat: <http://statistics.data.gov.uk/def/statistical-entity#>\nPREFIX mp: <http://statistics.gov.scot/def/measure-properties/>\nSELECT ?featurecode ?featurename ?areatypename ?date ?rank\nWHERE {\n  ?indicator qb:dataSet data:scottish-index-of-multiple-deprivation;\n              mp:rank ?rank;\n              dim:simdDomain dom:income;\n              sdmxd:refArea ?featurecode;\n              sdmxd:refPeriod ?period.\n\n              ?featurecode stat:code ?areatype;\n                rdfs:label ?featurename.\n              ?areatype rdfs:label ?areatypename.\n              ?period rdfs:label ?date.\n}"
    download_from_db(url = "https://statistics.gov.scot/sparql", 
                     path = query, local = "data-raw", filename = "scotgov_simd_income.csv")
  }
  else if (dataset == "scotgov_ur_classification") {
    query <- "PREFIX qb: <http://purl.org/linked-data/cube#>\nPREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>\nPREFIX data: <http://statistics.gov.scot/data/>\nPREFIX sdmxd: <http://purl.org/linked-data/sdmx/2009/dimension#>\nPREFIX stat: <http://statistics.data.gov.uk/def/statistical-entity#>\nPREFIX mp: <http://statistics.gov.scot/def/measure-properties/>\nPREFIX year:<http://reference.data.gov.uk/id/year/>\nSELECT ?featurecode ?featurename ?areatypename ?rank\nWHERE {\n  ?indicator qb:dataSet data:urban-rural-classification;\n              mp:rank ?rank;\n              sdmxd:refArea ?featurecode;\n              sdmxd:refPeriod year:2016.\n\n              ?featurecode stat:code ?areatype;\n                rdfs:label ?featurename.\n              ?areatype rdfs:label ?areatypename.\n}"
    download_from_db(url = "https://statistics.gov.scot/sparql", 
                     path = query, local = "data-raw", filename = "scotgov_ur_classification.csv")
  } else if (dataset == "ukgov_eng_oa_shapefile"){
    download_from_url(url = "https://opendata.arcgis.com/datasets", 
                      path = "ff8151d927974f349de240e7c8f6c140_0.zip?outSR=%7B%22latestWkid%22%3A3857%2C%22wkid%22%3A102100%7D", 
                      local = "data-raw/outputarea_shapefile",
                      filename = "ff8151d927974f349de240e7c8f6c140_0.zip")
  }else if (dataset == "ukgov_eng_lookup"){
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
    
    }
  else stop("Dataset not recognised.")
}
