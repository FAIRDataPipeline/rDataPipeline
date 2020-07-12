#' new_storage_location
#'
#' @param path e.g.
#' @param hash e.g.
#' @param storage_root e.g.
#' @param key key
#'
#' @export
#'
new_storage_location <- function(path,
                                 hash,
                                 storage_root,
                                 key) {

  post_data(table = "storage_location",
            data = list(path = path,
                        hash = hash,
                        storage_root = storage_root),
            key)
}
