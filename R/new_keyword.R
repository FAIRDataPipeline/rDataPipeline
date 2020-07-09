#' new_keyword
#'
#' @param object
#' @param keyphrase
#'
#' @export
#'
new_keyword <- function(keyphrase, object) {

  object_url <- get_url("object", list(id = object))

  post_data(table = "keyword",
            data =  list(keyphrase = keyphrase,
                         object = object_url),
            key)
}
