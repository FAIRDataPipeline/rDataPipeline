#' Post entry to namespace table
#'
#' Upload information to the \code{namespace} table in the data registry
#'
#' @param name a \code{string} specifying the name of the namespace
#'
#' @family new functions
#'
#' @export
#'
new_namespace <- function(name) {

  post_data(table = "namespace",
            data =  list(name = name))
}
