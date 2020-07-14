#' upload_data_product
#'
#' @param storage_root_id e.g.
#' @param path e.g.
#' @param component_name e.g.
#' @param filename e.g.
#' @param version e.g.
#' @param namespace_id e.g.
#' @param key key
#'
#' @export
#'
upload_data_product <- function(storage_root_id,
                                path,
                                component_name,
                                filename,
                                version,
                                namespace_id,
                                key) {

  product_storeId <- new_storage_location(
    path = file.path(path, filename),
    hash = get_hash(file.path("data-raw", path, filename)),
    storage_root_id = storage_root_id,
    key = key)

  product_objectId <- new_object(storage_location_id = product_storeId,
                                 key = key)

  if(grepl(".h5$", filename)) {
    components <- file_structure(filename)
  } else if(grepl(".toml$", filename)) {
    components <- data.frame(name = component_name)
  } else {
    stop("This code isn't ready for this filetype!")
  }

  for(i in seq_len(nrow(components))) {
    componentId <- new_object_component(name = components$name[i],
                                        object_id = product_objectId,
                                        key = key)
  }

  new_data_product(name = path,
                   version = version,
                   object_id = product_objectId,
                   namespace_id = namespace_id,
                   key = key)
}
