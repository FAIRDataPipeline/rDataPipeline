#' upload_submission_script
#'
#' @param storage_root_id e.g.
#' @param path e.g. "ScottishCovidResponse/SCRCdata"
#' @param hash e.g.
#' @param run_date e.g.
#' @param key key
#'
#' @export
#'
upload_submission_script <- function(storage_root_id,
                                     path,
                                     hash,
                                     text,
                                     run_date,
                                     key) {

  script_storeId <- new_storage_location(path = path,
                                         hash = hash,
                                         storage_root = storage_root_id,
                                         key = key)

  script_textFileId <- new_text_file(text = text)

  script_objectId <- new_object(storage_location_id = script_storeId,
                                key = key)

  list(script_storeId = script_storeId,
       script_textFileId = script_textFileId,
       script_objectId = script_objectId)
}
