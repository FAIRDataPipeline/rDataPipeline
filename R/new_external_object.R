#' new_external_object
#'
#' @param object_id
#' @param doi_or_unique_name
#' @param primary_not_supplement
#' @param release_date
#' @param title
#' @param description
#' @param source_id
#' @param original_store_id
#' @param version
#'
#' @export
#'
new_external_object <- function(object_id,
                                doi_or_unique_name,
                                primary_not_supplement,
                                release_date,
                                title,
                                description,
                                source_id,
                                original_store_id,
                                version) {

  object_url <- get_url("Object", list(id = object_id))
  source_url <- get_url("Source", list(id = source_id))
  storage_location_url <- get_url("StorageLocation", list(id = original_store_id))


  post_data(table = "ExternalObject",
            data = list(object_id = object_url,
                        doi_or_unique_name = doi_or_unique_name,
                        primary_not_supplement = primary_not_supplement,
                        release_date = release_date,
                        title = title,
                        description = description,
                        source_id = source_url,
                        original_store_id = storage_location_url,
                        version = version))
}
