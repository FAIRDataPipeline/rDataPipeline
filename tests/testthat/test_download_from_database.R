context("testing download_from_database")

#Endpoint
original_root <- "https://statistics.gov.scot/sparql.csv?query="

# SPARQL query
original_path <- "PREFIX qb: <http://purl.org/linked-data/cube#>
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

test_that("download_from_database downloads the correct file", {


# Download the file
download_from_database(source_root = original_root,
                       source_path = original_path,
                       filename = "0.1.0.csv",
                       path = "records/SARS-CoV-2/scotland/human-mortality")
  # This df is empty??
expect_true(file.exists("records/SARS-CoV-2/scotland/human-mortality/0.1.0.csv"))
df <- read.csv("records/SARS-CoV-2/scotland/human-mortality/0.1.0.csv")
expect_true(is.data.frame(df))

})

#totdo add friendly errors
test_that("download from database produces errors correctly", {
  expect_error(download_from_database())
  expect_error(download_from_database(source_root = original_root))
  expect_error(download_from_database(source_root = original_root,
               source_path = original_path))
  expect_error(download_from_database(source_root = Inf,
                                      source_path = -Inf,
                                      filename = "0.1.0.csv",
                                      path = "records/SARS-CoV-2/scotland/human-mortality"))
})

# Delete the file
file.remove("records/SARS-CoV-2/scotland/human-mortality/0.1.0.csv")
unlink("records", recursive = TRUE)

