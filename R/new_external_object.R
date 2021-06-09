#' Post entry to external_object table
#'
#' Upload information to the \code{external_object} table in the data registry
#'
#' @param doi_or_unique_name a \code{string} specifying the DOI or name of the
#' \code{external_object}
#' @param primary_not_supplement a boolean flag to indicate
#' whether the external object is the primary source (\code{TRUE}) or not
#' (\code{FALSE})
#' @param release_date the date-time that the \code{external_object} was
#' released *e.g.* \code{Sys.time()} or "2010-07-11 12:15:00 BST"
#' @param title a `string` specifying the title of the \code{external_object}
#' @param description (optional) a \code{string} containing a free text
#' description of the \code{external_object}
#' @param version a \code{string} specifying the version identifier of the
#' \code{external_object} (must conform to semantic versioning syntax)
#' @param object_uri a \code{string} specifying the URI of an entry in the
#' \code{object} table
#' @param source_uri a \code{string} specifying the URI of an entry in the
#' \code{source} table
#' @param original_store_uri (optional) a `string` specifying the URI of a
#' an entry in the \code{storage_location} table that references the original
#' location of an \code{external_object}
#'
#' @family new functions
#'
#' @export
#'
new_external_object <- function(doi_or_unique_name,
                                primary_not_supplement = TRUE,
                                release_date,
                                title,
                                description = "",
                                version,
                                object_uri,
                                source_uri,
                                original_store_uri = "") {

  post_data(table = "external_object",
            data = list(doi_or_unique_name = doi_or_unique_name,
                        primary_not_supplement = primary_not_supplement,
                        release_date = release_date,
                        title = title,
                        description = description,
                        version = version,
                        object = object_uri,
                        source = source_uri,
                        original_store = original_store_uri))
}
