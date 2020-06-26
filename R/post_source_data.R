#' post_source_data
#'
#' @param storage_type Name of storage type, e.g. ftp, https
#' @param storage_root Name of the storage root, e.g. "Boydorr"
#' @param storage_location Name of the storage_location, e.g. "Model File"
#' @param accessibility Name of accessibility type
#' @param source_type Name of the source type
#' @param source Name of the source
#' @param source_version_identifier
#' @param source_version_description Free text description of the source_version
#' @param responsible_person Name of the responsible person
#' @param source_version_supercedes
#' @param key GitHub key
#'
#' @export
#'
#'
post_source_data <- function(
  storage_type = "ftp",
  storage_root = "Boydorr",
  accessibility = "public",
  source_type = "database",
  source = "statistics.gov.scot",

  storage_location = "deaths-involving-coronavirus-covid-19.csv",
  source_version = "0.1.0",
  target_path = file.path(
    "human", "infection", "SARS-CoV-2", "scotland", "mortality",
    "deaths-involving-coronavirus-covid-19.csv"),
  hash = get_hash(file.path("data-raw",
                            "deaths-involving-coronavirus-covid-19.csv"))) {

  # Does storage_type exist in the database?
  if(!check_query("storage_type", list(name = storage_type)))
    stop("storage_type does not exist")

  # Does storage_root exist in the database?
  if(!check_query("storage_root", list(name = storage_root)))
    stop("storage_root does not exist")

  # Does accessibility exist in the database?
  if(!check_query("accessibility", list(name = accessibility)))
    stop("accessibility does not exist")

  # Does source_type exist in the database?
  if(!check_query("source_type", list(name = source_type)))
    stop("source_type does not exist")

  # Does source exist in the database?
  if(!check_query("source", list(name = source)))
    stop("source does not exist")

  # Does source_version already exist at this storage_location? If yes, then
  # check the existing entry has a matching hash, otherwise create a new entry
  does_version_exist <- check_query("storage_location",
                                    list(name = storage_location,
                                         version = source_version))

  if(does_version_exist) {
    does_hash_match <- check_query("storage_location",
                                   list(name = storage_location,
                                        version = source_version,
                                        hash = hash))
    if(!does_hash_match)
      stop("This version already exists in the database with a different hash.")

  } else {
    new_storage_location(
      name = storage_location,
      description = paste("Storage on", storage_root,
                          storage_type, "for",
                          storage_location,
                          "version", source_version),
      path = target_path,
      hash = hash,
      local_cache_url = "",
      responsible_person = responsible_person,
      storage_root = storage_root,
      key = key)
  }

  # Write new source_version
  new_source_version(version_identifier = source_data$source_version,
                     description = paste(data_product$data_product, "dataset"),
                     responsible_person = responsible_person,
                     supercedes = "",
                     source = source_data$source,
                     store = source_data$storage_location,
                     accessibility = source_data$accessibility,
                     key = key)

}
