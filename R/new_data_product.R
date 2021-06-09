#' Post entry to data_product table
#'
#' Upload information to the \code{data_product} table in the data registry
#'
#' @param name a \code{string} specifying the name of the \code{data_product}
#' @param version a \code{string} specifying the version identifier of the
#' \code{data_product} (must conform to semantic versioning syntax)
#' @param object_uri a \code{string} specifying the URI of the entry in the
#' \code{object} table
#' @param namespace_uri a \code{string} specifying the URI of the entry in the
#' \code{namespace} table
#'
#' @family new functions
#'
#' @export
#'
new_data_product <- function(name,
                             version,
                             object_uri,
                             namespace_uri) {

  post_data(table = "data_product",
            data =  list(name = name,
                         version = version,
                         object = object_uri,
                         namespace = namespace_uri))
}
