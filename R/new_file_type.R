#' Post entry to file_type table
#'
#' Upload information to the \code{file_type} table in the data registry
#' 
#' @keywords internal
#'
#' @param name a \code{string} specifying the name of the file type
#' @param extension a \code{string} specifying the filename extension
#' @param endpoint a \code{string} specifying the registry endpoint
#'
#' @family new functions
#'
new_file_type <- function(name,
                          extension,
                          endpoint = "http://127.0.0.1:8000/api/") {

  post_data(table = "file_type",
            data =  list(name = name,
                         extension = extension),
            endpoint = endpoint)
}
