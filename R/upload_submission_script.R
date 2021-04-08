#' Upload submission_script metadata to the data registry
#'
#' @param storage_root_id e.g.
#' @param path e.g. "ScottishCovidResponse/SCRCdata"
#' @param hash e.g.
#' @param text text
#' @param run_date e.g.
#'
#' @family upload functions
#'
#' @export
#'
upload_submission_script <- function(storage_root_id,
                                     path,
                                     hash,
                                     text,
                                     run_date) {

  script_textFileId <- new_text_file(text = text)

  tmp <- gsub("^.*/([0-9]+)/$", "\\1", script_textFileId)
  script_path <- paste0(tmp, "/?format=text")

  script_storeId <- new_storage_location(path = script_path,
                                         hash = hash,
                                         storage_root_id = storage_root_id)

  script_objectId <- new_object(storage_location_id = script_storeId)

  list(script_storeId = script_storeId,
       script_textFileId = script_textFileId,
       script_objectId = script_objectId)
}
