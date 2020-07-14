#' new_external_object
#'
#' @param doi_or_unique_name e.g.
#' @param primary_not_supplement e.g.
#' @param release_date e.g.
#' @param title e.g.
#' @param description e.g.
#' @param version e.g.
#' @param object_id e.g.
#' @param source_id e.g.
#' @param original_store_id e.g.
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
