#' upload_processing_script
#'
#' @param storage_root_id e.g.
#' @param path e.g. "ScottishCovidResponse/SCRCdata"
#' @param repo repo
#' @param hash e.g.
#' @param key key
#'
upload_processing_script <- function(storage_root_id,
                                     path,
                                     repo,
                                     hash = get_github_hash(repo),
                                     key) {

  script_storeId <- new_storage_location(path = path,
                                         hash = hash,
                                         storage_root = storage_root_id,
                                         key = key)

  new_object(storage_location_id = script_storeId,
             key = key)
}
