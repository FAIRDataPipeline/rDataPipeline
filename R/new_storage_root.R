#' Post entry to storage_root table
#'
#' Upload information to the \code{storage_root} table in the data registry
#'
#' @param root a \code{string} specifying the URI of a
#' \code{storage_location}, which when prepended to a \code{storage_location}
#' produces a complete URI to a file
#' @param local (optional) a \code{boolean} indicating whether the
#' \code{storage_root} is local or not
#' @param endpoint a \code{string} specifying the registry endpoint
#'
#' @family new functions
#'
new_storage_root <- function(root,
                             local,
                             endpoint = "http://localhost:8000/api/") {

  data <- list(root = root)

  if (!missing(local))
    data$local <- local

  post_data(table = "storage_root",
            data = data,
            endpoint = endpoint)
}
