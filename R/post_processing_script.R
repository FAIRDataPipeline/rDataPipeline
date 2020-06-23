#' post_processing_script
#'
#' @param storage_type Name of storage type, e.g. ftp, https
#' @param storage_root Name of the storage root, e.g. "Boydorr"
#' @param storage_location Name of the storage_location, e.g. "Model File"
#' @param accessibility Name of accessibility type
#' @param processing_script
#' @param script_version_identifier
#' @param responsible_person Name of the responsible person
#' @param script_version_supercedes
#' @param key GitHub key
#'
#' @export
#'
post_processing_script <- function(storage_type,
                                   storage_root,
                                   storage_location,
                                   accessibility,
                                   processing_script,
                                   script_version_identifier,
                                   responsible_person,
                                   script_version_supercedes,
                                   key) {

  arguments <- c("storage_type",
                 "storage_root",
                 "storage_location",
                 "accessibility",
                 "processing_script",
                 "responsible_person")

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

  new_processing_script_version(
    version_identifier = script_version_identifier,
    responsible_person = responsible_person,
    supercedes = script_version_supercedes,
    processing_script = processing_script,
    store = storage_location,
    accessibility = accessibility,
    data_product_versions = data_product_versions,
    key = key)
}
