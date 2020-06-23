#' new_storage_root
#'
#' @param name Name of the storage root, e.g. "Boydorr"
#' @param description Free text description of the storage root, e.g. "Boydorr
#' FTP server"
#' @param uri Link to the root of the storage, e.g. "ftp://srv/ftp/scrc"
#' @param type Name of storage type, e.g. ftp, https
#' @param key GitHub key
#'
#' @export
#'
new_storage_root <- function(name,
                             description,
                             uri,
                             type,
                             key) {

  available <- get_existing(table = "storage_type")

  if(any(type %in% available)) {
    type_url <- get_existing(table = "storage_type", key = type)$url

  } else {
    stop(
      paste(type, "does not exist. Please select from the following options",
            "or create a new entry using new_storage_type():\n",
            paste(available, collapse = "\n"))
    )
  }

  post_data(table = "storage_root",
            data = list(name = name,
                        description = description,
                        uri = uri,
                        type = type_url),
            key)
}
