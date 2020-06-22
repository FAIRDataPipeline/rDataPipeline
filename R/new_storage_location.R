#' new_storage_location
#'
#' @param name name of the storage_location, e.g. "Model File"
#' @param description free text description of the storage_location, e.g.
#' "Storage on Boydorr FTP for SCRC model"
#' @param path path to the data from the root of the store, e.g.
#' "models/my_model.txt"
#' @param hash SHA1 hash of the file
#' @param local_cache_url reference to a local store of the data, most likely
#' left blank
#' @param responsible_person reference to the responsible_person
#' @param store_root reference to the storage_root
#'
#' @export
#'
new_storage_location <- function(name,
                                 description,
                                 path,
                                 hash,
                                 local_cache_url,
                                 responsible_person,
                                 store_root) {

list(name = name,
     description = description,
     path = path,
     hash = hash,
     local_cache_url = local_cache_url,
     responsible_person = responsible_person,
     store_root = store_root)
}
