#' Post entry to keyword table
#'
#' Upload information to the \code{keyword} table in the data registry
#' 
#' @keywords internal
#'
#' @param object_url a \code{string} specifying the URL of an \code{object}
#' @param keyphrase a \code{string} a \code{string} containing a free text
#' key phrase
#' @param identifier (optional) a \code{string} specifying the URL of ontology
#' annotation to associate with this \code{keyword}
#' @param endpoint a \code{string} specifying the registry endpoint
#'
#' @family new functions
#'
new_keyword <- function(object_url,
                        keyphrase,
                        identifier,
                        endpoint = "http://127.0.0.1:8000/api/") {

  data <- list(object = object_url,
               keyphrase = keyphrase)

  if (!missing(identifier))
    data$identifier <- identifier

  post_data(table = "keyword",
            data = data,
            endpoint = endpoint)
}
