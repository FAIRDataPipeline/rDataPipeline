#' new_text_file
#'
#' @param text free text
#' @param key key
#'
#' @export
#'
new_text_file <- function(text, key) {

  post_data(
    table = "text_file",
    data =  list(text = text),
    key)
}
