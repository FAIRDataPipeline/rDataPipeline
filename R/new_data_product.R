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
#'
#' @family new functions
#'
#' @export
#'
new_data_product <- function(name,
                             version,
                             object_url,
                             namespace_url) {

  post_data(table = "data_product",
            data =  list(name = name,
                         version = version,
                         object = object_url,
                         namespace = namespace_url))
}
