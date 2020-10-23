#' upload_data_product
#'
#' @param storage_root_id e.g.
#' @param name e.g.
#' @param component_name (optional) used for toml files, not hdf5
#' @param processed_path e.g.
#' @param product_path e.g.
#' @param version e.g.
#' @param namespace_id e.g.
#' @param key API token from data.scrc.uk
#'
#' @export
#'
upload_data_product <- function(storage_root_id,
                                name,
                                component_name,
                                processed_path,
                                product_path,
                                version,
                                namespace_id,
                                key) {

  product_storeId <- new_storage_location(
    path = product_path,
    hash = get_hash(processed_path),
    storage_root_id = storage_root_id,
    key = key)

  product_objectId <- new_object(storage_location_id = product_storeId,
                                 key = key)

  if(grepl(".h5$", processed_path)) {
    components <- get_components(processed_path)
  } else if(grepl(".toml$", processed_path)) {
    components <- data.frame(name = component_name)
  } else {
    stop("Why is your data product not a toml or an h5 file?")
  }

  product_objectComponentId <- lapply(seq_along(components), function(i) {
    new_object_component(name = components[i],
                         object_id = product_objectId,
                         key = key)
  })

  product_dataProductId <- new_data_product(name = name,
                                            version = version,
                                            object_id = product_objectId,
                                            namespace_id = namespace_id,
                                            key = key)

  list(product_storeId = product_storeId,
       product_objectId = product_objectId,
       product_objectComponentId = product_objectComponentId,
       product_dataProductId = product_dataProductId)
}
