#' upload_source_data
#'
#' @param dataset e.g. "scottish deaths-involving-coronavirus-covid-19"
#' @param source e.g. "Scottish Government Open Data Repository"
#' @param source_root e.g. "https://statistics.gov.scot/sparql.json?query="
#' @param source_path e.g. some query
#' @param local_path e.g. "data-raw/deaths-involving-coronavirus-covid-19.csv"
#' @param storage_root e.g. "boydorr"
#' @param target_path e.g. "human/infection/SARS-CoV-2/scotland/mortality/deaths-involving-coronavirus-covid-19.csv"
#' @param download_date e.g. as.Date("2010-07-09", "%Y-%m-%d")
#' @param version e.g. 0
#' @param accessibility e.g. "public"
#' @param key
#'
#' @export
#'
upload_source_data <- function(dataset,
                               source,
                               source_root,
                               source_path,
                               local_path,
                               storage_root,
                               target_path,
                               download_date,
                               version,
                               key) {

  # Check if storage root already exists (root of where the source data is
  # being stored)
  if(check_exists("storage_root", list(name = storage_root))) {
    storageRootId <- get_url("storage_root", list(name = storage_root))

  } else {
    stop(paste0("The storage_root \"", storage_root,
                "\" does not currently exist in the data repository.",
                "Please run new_storage_root() to write a new entry."))
  }

  # Check if source already exists (where source data came from)
  if(check_exists("source", list(name = source))) {
    sourceId <- get_url("source", list(name = source))

  } else {
    stop(paste0("The source \"", source,
                "\" does not currently exist in the data repository.",
                "Please run new_source() to write a new entry."))
  }


  # download source data ----------------------------------------------------

  if(grepl("PREFIX", source_path) &
     grepl("WHERE", source_path) &
     grepl("SELECT", source_path)) {

  httr::GET(paste0(source_root,
                   utils::URLencode(source_path, reserved = TRUE)),
            httr::content_type("text/csv"),
            httr::write_disk(local_path, overwrite = TRUE))

  } else {

    stop("you still need to write the code to download csvs")
  }

  # upload original-source metadata to registry ------------------------------

  original_storeId <- new_storage_location(path = query, # query
                                           hash = get_hash(local_path),
                                           storage_root = original_storageRootId,
                                           key = key)



  # upload source data to store ---------------------------------------------

  upload_to <- file.path(get_entry("storage_root",
                                   list(name = "boydorr"))[[1]]$uri,
                         target_path)

  ####### need to automate this ########### !!!!!


  # upload source data metadata to registry ----------------------------------

  storageLocationId <- new_storage_location(path = target_path,
                                            hash = get_hash(local_path),
                                            storage_root = storageRootId,
                                            key = key)

  objectId <- new_object(storage_location = storageLocationId,
                         key = key)

  new_external_object(doi_or_unique_name = dataset,
                      primary_not_supplement = TRUE,
                      release_date = download_date,
                      title = dataset,
                      description = paste(dataset, "dataset"),
                      version = create_version_number(download_date, version),
                      object = objectId,
                      source = sourceId,
                      original_store = original_storeId,
                      key = key)
}
