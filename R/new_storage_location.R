#' new_storage_location
#'
#' @param name Name of the storage_location, e.g. "Model File"
#' @param description Free text description of the storage_location, e.g.
#' "Storage on Boydorr FTP for SCRC model"
#' @param path Path to the data from the root of the store, e.g.
#' "models/my_model.txt"
#' @param hash SHA1 hash of the file
#' @param local_cache_url Reference to a local store of the data, most likely
#' left blank
#' @param responsible_person Name of the responsible person
#' @param store_root Name of the storage root, e.g. "Boydorr"
#' @param key GitHub key
#'
#' @export
#'
new_storage_location <- function(name,
                                 description,
                                 path,
                                 hash,
                                 local_cache_url,
                                 responsible_person,
                                 storage_root,
                                 key) {

  rp_url <- get_responsible_person(responsible_person, key)
  root_url <- get_url("storage_root", list(name = storage_root))

  post_data(table = "storage_location",
            data = list(name = name,
                        description = description,
                        path = path,
                        hash = hash,
                        local_cache_url = local_cache_url,
                        responsible_person = rp_url,
                        store_root = root_url),
            key)
}
