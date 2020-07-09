#' new_external_object
#'
#' @param doi_or_unique_name
#' @param primary_not_supplement
#' @param release_date
#' @param title
#' @param description
#' @param version
#' @param object
#' @param source
#' @param original_store
#'
#' @export
#'
new_external_object <- function(doi_or_unique_name,
                                primary_not_supplement,
                                release_date,
                                title,
                                description,
                                version,
                                object,
                                source,
                                original_store) {

  object_url <- get_url("object", list(id = object))
  source_url <- get_url("source", list(id = source))
  storage_location_url <- get_url("storage_location", list(id = original_store))

  post_data(table = "external_object",
            data = list(doi_or_unique_name = doi_or_unique_name,
                        primary_not_supplement = primary_not_supplement,
                        release_date = release_date,
                        title = title,
                        description = description,
                        version = version,
                        object = object_url,
                        source = source_url,
                        original_store = storage_location_url),
            key)
}
