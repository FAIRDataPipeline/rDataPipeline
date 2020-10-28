#' Post to external_object table
#'
#' Upload information to the \code{external_object} table in the data registry
#'
#' @param doi_or_unique_name a \code{string} specifying the DOI or name of the
#' \code{external_object}
#' *e.g.* "scottish coronavirus-covid-19-management-information"
#' @param primary_not_supplement a boolean flag to indicate
#' whether the external object is the primary source (\code{TRUE}) or not
#' (\code{FALSE})
#' @param release_date the date-time that the \code{external_object} was
#' released *e.g.* \code{Sys.time()} or "2010-07-11 12:15:00 BST"
#' @param title a `string` specifying the title of the \code{external_object}
#' *e.g.* "scottish coronavirus-covid-19-management-information"
#' @param description (optional) a \code{string} containing a free text
#' description of the \code{external_object}
#' *e.g.* "scottish coronavirus-covid-19-management-information dataset"
#' @param version a \code{string} specifying the version identifier of the
#' \code{external_object} (must conform to semantic versioning syntax)
#'  *e.g.* "0.20100711.1"
#' @param object_id a \code{string} specifying the API URL of the
#' associated \code{object} table *e.g.* "https://data.scrc.uk/api/object/12/"
#' @param source_id a \code{string} specifying the API URL of the
#' associated \code{source} table *e.g.* "https://data.scrc.uk/api/source/9/"
#' @param original_store_id (optional) a `string` specifying the API URL of the
#' associated \code{storage_location} table that references the original
#' location of the \code{external_object}
#' *e.g.* "https://data.scrc.uk/api/storage_location/19/"
#' @param key API token from data.scrc.uk
#'
#' @export
#'
new_external_object <- function(doi_or_unique_name,
                                primary_not_supplement = TRUE,
                                release_date,
                                title,
                                description = "",
                                version,
                                object_id,
                                source_id,
                                original_store_id = "",
                                key) {

  post_data(table = "external_object",
            data = list(doi_or_unique_name = doi_or_unique_name,
                        primary_not_supplement = primary_not_supplement,
                        release_date = release_date,
                        title = title,
                        description = description,
                        version = version,
                        object = object_id,
                        source = source_id,
                        original_store = original_store_id),
            key)
}
