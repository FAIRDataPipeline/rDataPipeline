#' new_storage_root
#'
#' @param name name of the storage root, e.g. "Boydorr"
#' @param description free text description of the storage root, e.g. "Boydorr
#' FTP server"
#' @param uri link to the root of the storage, e.g. "ftp://srv/ftp/scrc"
#' @param type reference to the storage type used
#'
#' @export
#'
new_storage_root <- function(name,
                             description,
                             uri,
                             type) {

  list(name = name,
       description = description,
       uri = uri,
       type = type)
}
