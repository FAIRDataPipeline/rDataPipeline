#' upload_source_data
#'
#' @param doi_or_unique_name e.g. "scottish deaths-involving-coronavirus-covid-19"
#' @param original_source_id e.g. "https://data.scrc.uk/api/source/9/"
#' @param original_root_id e.g.
#' @param original_path e.g. query
#' @param primary_not_supplement e.g.
#' @param local_path e.g. "data-raw/human/infection/SARS-CoV-2/scotland/cases_and_management/20100711.0.csv"
#' @param storage_root_id e.g. "https://data.scrc.uk/api/storage_root/9/"
#' @param target_path e.g. "human/infection/SARS-CoV-2/scotland/cases_and_management/20100711.0.csv"
#' @param download_date e.g. as.POSIXct("2010-07-11 12:15:00", format = "%Y-%m-%d %H:%M:%S")
#' @param version e.g. "20100711.0"
#' @param key API token from data.scrc.uk
#'
#' @export
#'
upload_source_data <- function(doi_or_unique_name,
                               original_source_id,
                               original_root_id,
                               original_path,
                               primary_not_supplement = TRUE,
                               local_path,
                               storage_root_id,
                               target_path,
                               download_date,
                               version,
                               key) {

  # upload original-source metadata to registry ------------------------------

  hash <- get_hash(local_path)

  original_storeId <- new_storage_location(
    path = original_path,
    hash = hash,
    storage_root_id = original_root_id,
    key = key)

  # upload source data metadata to registry ----------------------------------

  source_storageLocationId <- new_storage_location(
    path = target_path,
    hash = hash,
    storage_root_id = storage_root_id,
    key = key)

  source_objectId <- new_object(storage_location_id = source_storageLocationId,
                                key = key)

  # Source data (e.g. *.csv) is defined as having a single component
  # called "table"
  source_objectComponentId <- new_object_component(name = "raw_data",
                                                   object_id = source_objectId,
                                                   key = key)

  source_externalObjectId <- new_external_object(
    doi_or_unique_name = doi_or_unique_name,
    primary_not_supplement = primary_not_supplement,
    release_date = download_date,
    title = doi_or_unique_name,
    description = paste(doi_or_unique_name, "dataset"),
    version = version,
    object_id = source_objectId,
    source_id = original_source_id,
    original_store_id = original_storeId,
    key = key)

  list(original_storeId = original_storeId,
       source_storageLocationId = source_storageLocationId,
       source_objectId = source_objectId,
       source_objectComponentId = source_objectComponentId,
       source_externalObjectId = source_externalObjectId)
}
