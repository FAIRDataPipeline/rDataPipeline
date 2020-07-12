#' upload_processing_script
#'
#' @param storage_root e.g.
#' @param path e.g. "ScottishCovidResponse/SCRCdata"
#' @param hash e.g.
#' @param key key
#'
upload_processing_script <- function(storage_root,
                                     path,
                                     hash = get_github_hash(path),
                                     key) {

  # Check if storage_root exists (where processing script is stored)
  if(check_exists("storage_root", list(name = storage_root))) {
    storageRootId <- get_url("storage_root", list(name = storage_root))

  } else {
    stop(paste0("The storage_root \"", storage_root,
                "\" does not currently exist in the data repository.",
                "Please run new_storage_root() to write a new entry."))
  }

  # Check if storage_root exists (where processing script is stored)
  if(check_exists("storage_root", list(name = storage_root)) &
     check_exists("storage_location", list(name = path))) {
    stop("Git repository already exists.")
  }


  # upload processing script metadata to registry ---------------------------

  script_storeId <- new_storage_location(path = path,
                                         hash = hash,
                                         storage_root = storage_rootId,
                                         key = key)

  new_object(storage_location_id = script_storeId,
             key = key)
}
