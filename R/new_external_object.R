#' new_external_object
#'
#' @param doi_or_unique_name
#' @param primary_not_supplement
#' @param release_date
#' @param title
#' @param description
#' @param version
#' @param object_id
#' @param source_id
#' @param original_store_id
#' @param key
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
