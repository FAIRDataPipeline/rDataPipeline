#' upload_data_product
#'
#' @param storage_root
#' @param path
#' @param dataset
#' @param filename
#' @param version
#' @param key
#'
#' @export
#'
upload_data_product <- function(storage_root,
                                path,
                                dataset,
                                filename,
                                version,
                                namespace = "SCRC",
                                key) {

  # Check if storage root already exists (root of where the data product is
  # being stored)
  if(check_exists("storage_root", list(name = storage_root))) {
    storageRootId <- get_url("storage_root", list(name = storage_root))

  } else {
    stop(paste0("The storage_root \"", storage_root,
                "\" does not currently exist in the data repository.",
                "Please run new_storage_root() to write a new entry."))
  }

  # Check if namespace already exists
  if(check_exists("namespace", list(name = namespace))) {
    namespaceId <- get_url("namespace", list(name = namespace))

  } else {
    stop(paste0("The namespace \"", namespace,
                "\" does not currently exist in the data repository.",
                "Please run new_namespace() to write a new entry."))
  }

  product_storeId <- new_storage_location(
    path = path,
    hash = get_hash(filename),
    storage_root = storageRootId,
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
                                        object = product_objectId,
                                        key = key)
  }

  new_data_product(name = paste(dataset, "dataset"),
                   version = version,
                   objectId = product_objectId,
                   namespace = namespaceId,
                   key = key)
}
