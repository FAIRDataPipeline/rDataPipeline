#' new_keyword
#'
#' @param object_id
#' @param phrase
#'
#' @export
#'
new_keyword <- function(object_id, phrase) {

  object_url <- get_url("Object", list(id = object_id))

  post_data(table = "Keyword",
            data =  list(object_id = object_url,
                         phrase = phrase))
}
