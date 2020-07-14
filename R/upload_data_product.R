#' upload_data_product
#'
#' @param storage_root_id e.g.
#' @param path e.g.
#' @param dataset e.g.
#' @param filename e.g.
#' @param version e.g.
#' @param namespace e.g.
#' @param key key
#'
#' @export
#'
upload_data_product <- function(storage_root_id,
                                path,
                                dataset,
                                filename,
                                version,
                                namespace,
                                key) {

  # Check if namespace already exists
  if(check_exists("namespace", list(name = namespace))) {
    namespaceId <- get_url("namespace", list(name = namespace))

  } else {
    stop(paste0("The namespace \"", namespace,
                "\" does not currently exist in the data repository.",
                "Please run new_namespace() to write a new entry."))
  }

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
    components <- data.frame(name = dataset)
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
                   namespace_id = namespaceId,
                   key = key)
}
