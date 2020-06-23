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

  type_url <- get_url(table = "storage_type", key = type)

  post_data(table = "storage_root",
            data = list(name = name,
                        description = description,
                        uri = uri,
                        type = type_url),
            key)
}
