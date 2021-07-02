#' Post entry to keyword table
#'
#' Upload information to the \code{keyword} table in the data registry
#'
#' @param object_url a \code{string} specifying the URL of an \code{object}
#' @param keyphrase a \code{string} a \code{string} containing a free text
#' key phrase
#' @param identifier (optional) a \code{string} specifying the URL of ontology
#' annotation to associate with this \code{Keyword}
#'
#' @family new functions
#'
#' @export
#'
new_keyword <- function(object_url,
                        keyphrase,
                        identifier) {

  data <- list(object = object_url,
               keyphrase = keyphrase)

  if (!missing(identifier))
    data$identifier <- identifier

  post_data(table = "keyword",
            data = data)
}
