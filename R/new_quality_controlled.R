#' Post entry to quality_controlled table
#'
#' Upload information to the \code{quality_controlled} table in the data registry
#'
#' @param object_url a \code{string} specifying the URL of an \code{object}
#'
#' @family new functions
#'
#' @export
#'
new_quality_controlled <- function(object_url) {

  post_data(table = "quality_controlled",
            data = list(object = object_url))
}
