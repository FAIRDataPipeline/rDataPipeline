#' get_h5_from_object_id
#'
#' @param obj_id object ID
#' @param data_dir Data Directory
#' @return H5 file path after downloading
#'
#' @export
#'

get_h5_from_object_id <-function(obj_id, data_dir = "data-raw"){
    if(!dir.exists(data_dir))
      stop("Data Directory does not exist")
    obj <- get_entity("object", obj_id)
    if(is.null(obj))
      stop("Object does not exist")
    storage_location <- get_storage_location(basename(obj$storage_location))
    if(is.null(storage_location))
      stop("There was a problem with the storage location")
    storage_root <- get_entity("storage_root", basename(storage_location$storage_root))
    if(is.null(storage_root)){
      stop("There was a problem with the storage root")
    }else if(! grepl("ftp://", storage_root$root)){
      stop("This function can only handle FTP Servers")
    }
    url <- file.path(storage_root$root, storage_location$path)
    dest_path <- file.path(data_dir, basename(storage_location$path))
    download.file(url, dest_path, mode ="wb")
    file.path(dest_path)
}
