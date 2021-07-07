#' findme
#'
#' Returns metadata associated with the calculated hash of a target file.
#' When multiple entries exist in the data registry all are returned.
#' These entries can be filtered, when \code{filter} is \code{TRUE}, by
#' attempting to match \code{file} to the Storage Location entry in the data
#' registry. The deeper the file path, the better the match.
#'
#' @param file file path
#' @param endpoint endpoint
#' @param filter when \code{TRUE} (default), attempts to match \code{file}
#' to a storage location
#'
findme <- function(file, endpoint, filter = TRUE) {

  hash <- get_file_hash(file)
  msg <- paste("hash:", hash)

  if (grepl("localhost", endpoint)) run_server()

  # There could be multiple Storage Locations with the same hash

  storage_location <- get_entry("storage_location", list(hash = hash))

  if (is.null(names(storage_location)))
    multiple_entries <- TRUE

  if (filter) {
    if (multiple_entries) {
      # Find a match
      index <- lapply(storage_location, function(x)
        grepl(file, x$path) | grepl(x$path, file)) %>%
        unlist() %>%
        which()
      storage_location <- storage_location[[index]]
    }
  }

  # If only a single entry is returned, turn it into a list
  if (!is.null(names(storage_location)))
    storage_location <- list(storage_location)

  for (y in seq_along(storage_location)) {
    this_storagelocation <- storage_location[[y]]
    root <- get_entity(this_storagelocation$storage_root)$root
    storage_id <- extract_id(this_storagelocation$url)
    object <- get_entry("object",
                        list(storage_location = storage_id))
    full_path <- paste0(root, this_storagelocation$path)
    msg <- paste(msg, "\n\n- path:", full_path)

    # There could be multiple Objects attached to the same Storage Location

    for (z in seq_along(object)) {
      data_product_url <- object[[z]]$data_product
      data_product <- get_entity(data_product_url)
      description <- object[[z]]$description
      components_url <- object[[z]]$components
      components <- lapply(components_url, function(w)
        get_entity(w)$name)
      namespace <- get_entity(data_product$namespace)$name
      user <- get_entity(data_product$updated_by)$full_name

      msg <- paste(msg, "\n\n  - data product:", data_product$name)
      msg <- paste(msg, "\n    description:", description)
      msg <- paste(msg, "\n    version:", data_product$version)
      msg <- paste(msg, "\n    namespace:", namespace)
      msg <- paste(msg, "\n    updated by:", user)
      msg <- paste(msg, "\n    last updated:", data_product$last_updated)

      for(v in seq_along(components)) {
        if (v == 1) {
          msg <- paste(msg, "\n    components:", components[[v]])
        } else {
          msg <- paste(msg, "\n               ", components[[v]])
        }
      }

      issues_url <- object[[z]]$issues
    }


  }

  cat(msg)
}
