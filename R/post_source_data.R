#' post_source_data
#'
#' @param name Name of dataset
#' @param description Description of dataset
#' @param hash SHA1 hash of the file
#' @param responsible_person Name of the responsible person
#' @param key GitHub key
#' @param storage_type Name of storage type, e.g. ftp, https
#' @param storage_root Name of the storage root, e.g. "Boydorr"
#' @param storage_location Name of the storage_location, e.g. "Model File"
#' @param accessibility Name of accessibility type
#' @param source_type Name of the source type
#' @param source Name of the source
#' @param source_version Version identifier of the source_version, e.g. 1
#' @param source_version_description Free text description of the source_version
#'
#' @export
#'
post_source_data <- function(name,
                             description,
                             hash,
                             responsible_person,
                             key,
                             storage_type,
                             storage_root,
                             storage_location,
                             accessibility,
                             source_type,
                             source,
                             source_version,
                             source_version_identifier,
                             source_version_description,
                             source_version_supercedes) {

  arguments <- c("storage_type",
                 "storage_root",
                 "responsible_person",
                 "source_type",
                 "source",
                 "accessibility")

  for(i in seq_along(arguments)) {
    available <- get_existing(table = arguments[i])

    if(!any(get(arguments[i]) %in% available))
      stop(
        paste(get(arguments[i]), "does not exist. Please select from the",
              "following options or create a new entry using",
              paste0("new_", arguments[i], "()"), ":\n",
              paste(available, collapse = "\n"))
      )
  }

  new_storage_location(
    name = name,
    description = description,
    path = storage_location,
    hash = hash,
    local_cache_url = "",
    responsible_person = responsible_person,
    storage_root = storage_root,
    key = key)

  new_source_version(
    version_identifier = source_version_identifier,
    description = source_version_description,
    responsible_person = responsible_person,
    supercedes = source_version_supercedes,
    source = source,
    store = storage_location,
    accessibility = accessibility,
    key = key)

}
