#' upload_external_data
#'
#' @export
#'
upload_external_data <- function(source_name,
                                 source_abbreviation,
                                 source_website,

  doi_or_unique_name,
                                 original_source_id,
                                 original_root_id,
                                 source_path,
                                 primary_not_supplement = TRUE,
                                 local_path,
                                 storage_root_id,
                                 target_path,
                                 download_date,
                                 version) {

  # Add the website to the data registry (e.g. home page of the database)
  original_sourceId <- new_source(
    name = source_name,
    abbreviation = source_abbreviation,
    website = source_website)

  # upload original-source metadata to registry ------------------------------

  hash <- get_file_hash(local_path)

  original_storeId <- new_storage_location(
    path = source_path,
    hash = hash,
    storage_root_id = original_root_id)

  # upload source data metadata to registry ----------------------------------

  source_storageLocationId <- new_storage_location(
    path = target_path,
    hash = hash,
    storage_root_id = storage_root_id)

  source_objectId <- new_object(storage_location_id = source_storageLocationId)

  # Source data (e.g. *.csv) is defined as having a single component
  # called "table"
  source_objectComponentId <- new_object_component(name = "raw_data",
                                                   object_id = source_objectId)

  source_externalObjectId <- new_external_object(
    doi_or_unique_name = doi_or_unique_name,
    primary_not_supplement = primary_not_supplement,
    release_date = download_date,
    title = doi_or_unique_name,
    description = paste(doi_or_unique_name, "dataset"),
    version = version,
    object_id = source_objectId,
    source_id = original_source_id,
    original_store_id = original_storeId)

  list(original_storeId = original_storeId,
       source_storageLocationId = source_storageLocationId,
       source_objectId = source_objectId,
       source_objectComponentId = source_objectComponentId,
       source_externalObjectId = source_externalObjectId)

}
