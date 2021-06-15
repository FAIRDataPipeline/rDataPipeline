#' Post entry to storage_location table
#'
#' Upload information to the \code{storage_location} table in the data registry
#'
#' @param path a \code{string} specifying the path from the \code{storage_root}
#' URI to the item location, which when appended to \code{storage_root} URI
#' produces a complete URL
#' @param hash a \code{string} specifying the SHA1 hash of a file stored in
#' `storage_location`
#' @param storage_root_url a \code{string} specifying the URL of an entry in
#' the \code{storage_root} table
#'
#' @family new functions
#'
#' @export
#'
new_storage_location <- function(path,
                                 hash,
                                 storage_root_url) {

  post_data(table = "storage_location",
            data = list(path = path,
                        hash = hash,
                        storage_root = storage_root_url))
}
