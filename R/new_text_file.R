#' new_text_file
#'
#' @export
#'
new_text_file <- function(text) {

  post_data(
    table = "text_file",
    data =  list(text = text),
    key)
}
