#' Post entry to storage_root table
#'
#' Upload information to the \code{storage_root} table in the data registry
#'
#' @param name a \code{string} specifying the name of the \code{storage_root}
#' @param root a \code{string} specifying the URI of a
#' \code{storage_location}, which when prepended to a \code{storage_location}
#' produces a complete URI to a file
#' @param accessibility (optional) an \code{integer} value for the
#' accessibility enum, where 0 is public (default) and 1 is private
#'
#' @family new functions
#'
#' @export
#'
new_storage_root <- function(name,
                             root,
                             accessibility = 0) {

  if (!grepl("\\/$", root))
    root <- paste0(root, "/")

  post_data(table = "storage_root",
            data = list(name = name,
                        root = root,
                        accessibility = accessibility))
}
