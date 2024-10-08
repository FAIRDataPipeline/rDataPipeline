#' Post entry to quality_controlled table
#'
#' Upload information to the \code{quality_controlled} table in the data
#' registry
#' 
#' @keywords internal
#'
#' @param object_url a \code{string} specifying the URL of an \code{object}
#' @param endpoint a \code{string} specifying the registry endpoint
#'
#' @family new functions
#'
new_quality_controlled <- function(object_url,
                                   endpoint = "http://127.0.0.1:8000/api/") {

  post_data(table = "quality_controlled",
            data = list(object = object_url),
            endpoint = endpoint)
}
