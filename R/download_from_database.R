#' Download source file from database
#'
#' @param source_root a \code{string} specifying the source root
#' @param source_path a \code{string} specifying the source path
#' @param path a \code{string} specifying where the file will be saved
#' @param filename a \code{string} specifying the filename (the name given to
#' the saved file)
#' @param overwrite a \code{boolean} specifying whether or not the file should
#' be overwritten if it already exists
#'
#' @family download functions
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # Endpoint
#' original_root <- "https://statistics.gov.scot/sparql.csv?query="
#'
#' # SPARQL query
#' original_path <- "PREFIX qb: <http://purl.org/linked-data/cube#>
#'    PREFIX data: <http://statistics.gov.scot/data/>
#'    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
#'    PREFIX dim: <http://purl.org/linked-data/sdmx/2009/dimension#>
#'    PREFIX sdim: <http://statistics.gov.scot/def/dimension/>
#'    PREFIX stat: <http://statistics.data.gov.uk/def/statistical-entity#>
#'    PREFIX mp: <http://statistics.gov.scot/def/measure-properties/>
#'    SELECT ?featurecode ?featurename ?areatypename ?date ?cause ?location ?gender ?age ?type ?count
#'    WHERE {
#'      ?indicator qb:dataSet data:deaths-involving-coronavirus-covid-19;
#'                  mp:count ?count;
#'                 qb:measureType ?measType;
#'                  sdim:age ?value;
#'                  sdim:causeofdeath ?causeDeath;
#'                  sdim:locationofdeath ?locDeath;
#'                  sdim:sex ?sex;
#'                  dim:refArea ?featurecode;
#'                  dim:refPeriod ?period.
#'
#'                  ?measType rdfs:label ?type.
#'                  ?value rdfs:label ?age.
#'                  ?causeDeath rdfs:label ?cause.
#'                  ?locDeath rdfs:label ?location.
#'                  ?sex rdfs:label ?gender.
#'                  ?featurecode stat:code ?areatype;
#'                  rdfs:label ?featurename.
#'                  ?areatype rdfs:label ?areatypename.
#'                  ?period rdfs:label ?date.
#'           }"
#'
#' # Download the file
#' download_from_database(source_root = original_root,
#'                        source_path = original_path,
#'                        filename = "0.1.0.csv",
#'                        path = "records/SARS-CoV-2/scotland/human-mortality")
#'
#' # Delete the file
#' file.remove("records/SARS-CoV-2/scotland/human-mortality/0.1.0.csv")
#' }
#'
download_from_database <- function(source_root,
                                   source_path,
                                   path,
                                   filename,
                                   overwrite = FALSE) {
  # Generate directory structure
  if(!file.exists(path)) dir.create(path, recursive = TRUE)
  if(missing(path)) path <- ""

  # Download file
  httr::GET(paste0(source_root,
                   utils::URLencode(source_path, reserved = TRUE)),
            httr::content_type("text/csv"),
            httr::write_disk(file.path(path, filename), overwrite = overwrite))
}
