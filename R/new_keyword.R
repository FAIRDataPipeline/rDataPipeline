#' new_keyword
#'
#' Upload information to the \code{keyword} table in the data registry
#'
#' @param object_id a \code{string} specifying the API URL of the
#' associated \code{object} table *e.g.* "https://data.scrc.uk/api/object/31817/"
#' @param keyphrase a \code{string} specifying a single keyword or keyphrase
#' *e.g.* "covid-19" or "cardiovascular diseases"
#' @param key API token from data.scrc.uk
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
