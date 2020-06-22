#' new_storage_location
#'
#' @param name
#' @param description
#' @param path
#' @param hash
#' @param local_cache_url
#' @param responsible_person
#' @param store_root
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
