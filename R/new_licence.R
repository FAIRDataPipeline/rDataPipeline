#' Post entry to licence table
#'
#' Upload information to the \code{licence} table in the data registry
#' 
#' @keywords internal
#'
#' @param object_url a \code{string} specifying the URL of an \code{object}
#' @param licence_info a free text \code{string} containing information about
#' the \code{licence}
#' @param endpoint a \code{string} specifying the registry endpoint
#'
#' @family new functions
#'
new_licence <- function(object_url,
                        licence_info,
                        endpoint = "http://127.0.0.1:8000/api/") {

  post_data(table = "licence",
            data =  list(object = object_url,
                         licence_info = licence_info),
            endpoint = endpoint)
}
