#' new_storage_location
#'
#' @param id
#' @param uri
#' @param text
#' @param hash
#' @param storage_root
#' @param accessibility_id
#'
#' @export
#'
new_storage_location <- function(id,
                                 uri,
                                 text,
                                 hash,
                                 storage_root,
                                 accessibility_id) {

  accessibility_url <- get_url("Accessibility", list(id = accessibility_id))

  post_data(table = "StorageLocation",
            data = list(id = id,
                        uri = uri,
                        text = text,
                        hash = hash,
                        storage_root = storage_root,
                        accessibility_id = accessibility_url))
}
