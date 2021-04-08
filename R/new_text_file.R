#' Post entry to text_file table
#'
#' Upload information to the \code{text_file} table in the data registry
#'
#' @param text a \code{string} containing a free text *i.e.* scripts that are
#' only a few lines long and don't have a home elsewhere
#'
#' @family new functions
#'
#' @export
#'
new_text_file <- function(text) {

  post_data(
    table = "text_file",
    data =  list(text = text))
}
