#' new_storage_type
#'
#' @param name Name of storage type, e.g. ftp, https
#' @param description free text description of the storage type, e.g. "File
#' Transfer Protocol"
#'
#' @export
#'
new_storage_type <- function(name,
                             description) {
  list(name = name,
       description = description)
}
