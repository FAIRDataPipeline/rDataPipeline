#' Post entry to storage_location table
#'
#' Upload information to the \code{storage_location} table in the data registry
#' 
#' @keywords internal
#'
#' @param path a \code{string} specifying the path from the \code{storage_root}
#' URI to the item location, which when appended to \code{storage_root} URI
#' produces a complete URL
#' @param hash a \code{string} specifying the SHA1 hash of a file stored in
#' `storage_location`
#' @param public a \code{boolean} indicating whether the \code{storage_location}
#' is public or not (default is \code{TRUE})
#' @param storage_root_url a \code{string} specifying the URL of an entry in
#' the \code{storage_root} table
#' @param endpoint a \code{string} specifying the registry endpoint
#'
#' @family new functions
#'
new_storage_location <- function(path,
                                 hash,
                                 public,
                                 storage_root_url,
                                 endpoint = "http://127.0.0.1:8000/api/") {
  data <- list(path = path,
               hash = hash,
               storage_root = storage_root_url)

  if (!missing(public))
    data$public <- public

  post_data(table = "storage_location",
            data = data,
            endpoint = endpoint)
}
