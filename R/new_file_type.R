#' Post entry to file_type table
#'
#' Upload information to the \code{file_type} table in the data registry
#'
#' @param name a \code{string} specifying the name of the file type
#' @param extension a \code{string} specifying the filename extension
#'
#' @family new functions
#'
#' @export
#'
new_file_type <- function(name,
                          extension) {

  post_data(table = "file_type",
            data =  list(name = name,
                         extension = extension))
}
