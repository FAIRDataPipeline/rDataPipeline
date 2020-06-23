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
post_source_data <- function(storage_type,
                             storage_root,
                             storage_location,
                             accessibility,
                             source_type,
                             source,
                             source_version_identifier,
                             source_version_description,
                             responsible_person,
                             source_version_supercedes,
                             key) {

  arguments <- c("storage_type",
                 "storage_root",
                 "storage_location",
                 "accessibility",
                 "source_type",
                 "source",
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
