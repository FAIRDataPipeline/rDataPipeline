#' Post entry to data_product table
#'
#' Upload information to the \code{data_product} table in the data registry
#'
#' @param name a \code{string} specifying the name of the \code{data_product}
#' @param version a \code{string} specifying the version identifier of the
#' \code{data_product} (must conform to semantic versioning syntax)
#' @param object_url a \code{string} specifying the URL of the entry in the
#' \code{object} table
#' @param namespace_url a \code{string} specifying the URL of the entry in the
#' \code{namespace} table
#' @param endpoint a \code{string} specifying the registry endpoint
#'
#' @family new functions
#'
new_data_product <- function(name,
                             version,
                             object_url,
                             namespace_url,
                             endpoint = "http://localhost:8000/api/") {

  post_data(table = "data_product",
            data =  list(name = name,
                         version = version,
                         object = object_url,
                         namespace = namespace_url),
            endpoint = endpoint)
}
