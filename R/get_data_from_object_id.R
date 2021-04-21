#' Download TOML/H5 file
#'
#' @param obj_id an \code{integer} specifying the id of an entry
#' @param data_dir a \code{string} specifying the directory in which the file
#' should be saved
#'
#' @return Returns a \code{list} comprising two elements
#' \itemize{
#' \item{downloaded_to}{absolute path of TOML/H5 file after downloading}
#' \item{components}{TOML/H5 file components}
#' }
#' @export
#' @keywords internal
#'
get_data_from_object_id <- function(obj_id, data_dir = "data-raw"){
  # If data_dir doesn't exist, create it
  if(!dir.exists(data_dir)) dir.create(data_dir, recursive = TRUE)

  obj <- get_entity(obj_id)
  if(is.null(obj)) stop("Object does not exist")

  storage_location <- get_storage_location(obj$storage_location)
  if(is.null(storage_location))
    stop("There was a problem with the storage location")

  storage_root <- get_entity("storage_root", basename(storage_location$storage_root))
  if(is.null(storage_root)){
    stop("There was a problem with the storage root")}
  # }else if(! grepl("ftp://", storage_root$root)){
  #   stop("This function can only handle FTP Servers")
  # }

  url <- file.path(storage_root$root, storage_location$path)
  dest_path <- file.path(data_dir, basename(storage_location$path))

  # If file already exists at this location, return a message. Otherwise,
  # download the file and return it's location and a list of components
  if(file.exists(dest_path)) {
    message("File already exists at this location")
  } else {
    download.file(url, dest_path, mode = "wb")
  }

  components <- lapply(obj$components, function(x) {
    tmp <- httr::GET(x) %>%
      httr::content(as = "text", encoding = "UTF-8") %>%
      jsonlite::fromJSON(simplifyVector = FALSE)
    tmp$name
  })

  return(list(downloaded_to = normalizePath(dest_path),
              components = unlist(components)))
}
