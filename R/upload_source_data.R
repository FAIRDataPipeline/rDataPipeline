#' upload_source_data
#'
#' @param doi_or_unique_name e.g. "scottish deaths-involving-coronavirus-covid-19"
#' @param original_source_id e.g. "https://data.scrc.uk/api/source/9/"
#' @param original_root_id e.g.
#' @param original_path e.g. query
#' @param local_path e.g. "data-raw/human/infection/SARS-CoV-2/scotland/cases_and_management/20100711.0.csv"
#' @param storage_root_id e.g. "https://data.scrc.uk/api/storage_root/9/"
#' @param target_path e.g. "human/infection/SARS-CoV-2/scotland/cases_and_management/20100711.0.csv"
#' @param download_date e.g. as.POSIXct("2010-07-11 12:15:00", format = "%Y-%m-%d %H:%M:%S")
#' @param version e.g. "20100711.0"
#' @param key key
#'
#' @export
#'
upload_source_data <- function(doi_or_unique_name,
                               original_source_id,
                               original_root_id,
                               original_path,
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

  storageLocationId <- new_storage_location(path = target_path,
                                            hash = hash,
                                            storage_root_id = storage_root_id,
                                            key = key)

  objectId <- new_object(storage_location_id = storageLocationId,
                         key = key)

  new_external_object(doi_or_unique_name = doi_or_unique_name,
                      primary_not_supplement = TRUE,
                      release_date = download_date,
                      title = doi_or_unique_name,
                      description = paste(doi_or_unique_name, "dataset"),
                      version = create_version_number(download_date, version),
                      object_id = objectId,
                      source_id = source_id,
                      original_store_id = original_storeId,
                      key = key)
}
