#' new_storage_type
#'
#' @param name Name of storage type, e.g. ftp, https
#' @param description Free text description of the storage type, e.g. "File
#' Transfer Protocol"
#' @param key GitHub key
#'
#' @export
#'
new_storage_type <- function(name,
                             description,
                             key) {

  post_data(table = "storage_type",
            data = list(name = name,
                        description = description),
            key)
}
