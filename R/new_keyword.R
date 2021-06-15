#' Post entry to keyword table
#'
#' Upload information to the \code{keyword} table in the data registry
#'
#' @param object_url a \code{string} specifying the URL of an \code{object}
#' @param keyphrase a \code{string} a \code{string} containing a free text
#' key phrase
#'
#' @family new functions
#'
#' @export
#'
new_keyword <- function(object_url,
                        keyphrase) {

  post_data(table = "keyword",
            data =  list(object = object_url,
                         keyphrase = keyphrase))
}
