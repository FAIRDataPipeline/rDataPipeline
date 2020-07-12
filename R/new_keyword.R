#' new_keyword
#'
#' @param object_id e.g.
#' @param keyphrase e.g.
#' @param key key
#'
#' @export
#'
new_keyword <- function(keyphrase,
                        object_id,
                        key) {

  post_data(table = "keyword",
            data =  list(keyphrase = keyphrase,
                         object = object_id),
            key)
}
