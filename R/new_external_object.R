#' new_external_object
#'
#' @param doi_or_unique_name e.g. "scottish coronavirus-covid-19-management-information"
#' @param primary_not_supplement e.g. TRUE
#' @param release_date e.g. "2010-07-11 12:15:00 BST"
#' @param title e.g. "scottish coronavirus-covid-19-management-information"
#' @param description e.g. "scottish coronavirus-covid-19-management-information dataset"
#' @param version e.g. "20100711.0"
#' @param object_id e.g. "https://data.scrc.uk/api/object/12/"
#' @param source_id e.g. "https://data.scrc.uk/api/source/9/"
#' @param original_store_id e.g. "https://data.scrc.uk/api/storage_location/19/"
#' @param key key
#'
#' @export
#'
new_external_object <- function(doi_or_unique_name,
                                primary_not_supplement,
                                release_date,
                                title,
                                description,
                                version,
                                object_id,
                                source_id,
                                original_store_id,
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
