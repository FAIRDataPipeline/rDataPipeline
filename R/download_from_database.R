#' download_from_database
#'
#' @param source_root a \code{string} specifying the
#' @param source_path a \code{string} specifying the
#' @param filename a \code{string} specifying the
#' @param path a \code{string} specifying the
#' @param overwrite overwrite
#'
#' @export
#'
#' @examples
#' \dontrun{
#' \donttest{
#' original_root <- "https://statistics.gov.scot/sparql.csv?query="
#' original_path <- "PREFIX qb: <http://purl.org/linked-data/cube#>
#' PREFIX data: <http://statistics.gov.scot/data/>
#' PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
#' PREFIX mp: <http://statistics.gov.scot/def/measure-properties/>
#' PREFIX dim: <http://purl.org/linked-data/sdmx/2009/dimension#>
#' PREFIX sdim: <http://statistics.gov.scot/def/dimension/>
#' PREFIX stat: <http://statistics.data.gov.uk/def/statistical-entity#>
#' SELECT ?featurecode ?featurename ?date ?measure ?variable ?count
#' WHERE {
#'   ?indicator qb:dataSet data:coronavirus-covid-19-management-information;
#'              dim:refArea ?featurecode;
#'               dim:refPeriod ?period;
#'               sdim:variable ?varname;
#'               qb:measureType ?type.
#' {?indicator mp:count ?count.} UNION {?indicator mp:ratio ?count.}
#'
#'   ?featurecode <http://publishmydata.com/def/ontology/foi/displayName> ?featurename.
#'   ?period rdfs:label ?date.
#'   ?varname rdfs:label ?variable.
#'   ?type rdfs:label ?measure.
#' }"
#'
#' source_filename <- "1.20200716.0.csv"
#' product_name <- paste("human", "infection", "SARS-CoV-2", "scotland",
#' "cases_and_management", sep = "/")
#' local_path <- file.path("data-raw", product_name)
#'
#' download_from_database(source_root = original_root,
#'                        source_path = original_path,
#'                        filename = source_filename,
#'                        path = local_path)
#' }}
#'
download_from_database <- function(source_root,
                                   source_path,
                                   filename,
                                   path,
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
