#' findme
#'
#' Returns metadata associated with the calculated hash of a target file.
#' When multiple entries exist in the data registry all are returned.
#'
#' @param file file path
#' @param endpoint endpoint
#'
#' @export
#'
findme <- function(file, endpoint) {

  hash <- get_file_hash(file)
  msg <- paste("hash:", hash)

  # There could be multiple Storage Locations with the same hash
  storage_location <- get_entry("storage_location", list(hash = hash))

  # If only a single entry is returned, turn it into a list
  if (!is.null(names(storage_location)))
    storage_location <- list(storage_location)

  for (y in seq_along(storage_location)) {
    this_storagelocation <- storage_location[[y]]
    root <- get_entity(this_storagelocation$storage_root)$root
    msg <- paste(msg, "\nroot:", root)
    storage_id <- extract_id(this_storagelocation$url, endpoint = endpoint)
    object <- get_entry("object",
                        list(storage_location = storage_id))
    location <- this_storagelocation$path
    msg <- paste(msg, "\nlocation:", location)

    # There could be multiple Objects attached to the same Storage Location

    for (z in seq_along(object)) {
      data_product_url <- object[[z]]$data_product
      assertthat::assert_that(length(data_product_url) == 1)
      data_product <- get_entity(data_product_url[[1]])
      description <- object[[z]]$description
      components_url <- object[[z]]$components
      components <- lapply(components_url, function(w)
        get_entity(w)$name)
      issues <- lapply(components_url, function(w) {
        issues_url <- get_entity(w)$issues
        lapply(issues_url, function(i) {
          description <- get_entity(i)$description
          severity <- get_entity(i)$severity
          paste0(description, " (severity ", severity, ")")
        }) %>% unlist()
      })
      namespace <- get_entity(data_product$namespace)$name
      user <- get_entity(data_product$updated_by)$full_name

      if (z == 1) {
        msg <- paste(msg, "\n\ndata products:")
      } else {
        msg <- paste(msg, "\n")
      }
      msg <- paste(msg, "\n-", data_product$name)
      msg <- paste(msg, "\n  description:", description)
      msg <- paste(msg, "\n  version:", data_product$version)
      msg <- paste(msg, "\n  namespace:", namespace)
      msg <- paste(msg, "\n  updated by:", user)
      msg <- paste(msg, "\n  last updated:", data_product$last_updated)
      msg <- paste(msg, "\n  components:")

      for (v in seq_along(components)) {
        msg <- paste(msg, "\n   -", components[[v]])
        if (!is.null(issues[[v]]))
          msg <- paste(msg, "\n     issues:", issues[[v]])
      }
    }
  }

  cat(msg)
  invisible(TRUE)
}
