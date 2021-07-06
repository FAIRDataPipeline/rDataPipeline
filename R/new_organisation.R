#' Post entry to organisation table
#'
#' Upload information to the \code{organisation} table in the data registry
#'
#' @param name a \code{string} specifying the name of the \code{organisation}
#' @param ror (optional) a unique 9-character \code{string} representing the
#' ROR ID of the \code{organisation} (https://ror.org)
#' @param endpoint a \code{string} specifying the registry endpoint
#'
#' @family new functions
#'
#' @export
#'
new_organisation <- function(name,
                             ror,
                             endpoint = "http://localhost:8000/api/") {

  post_data(table = "organisation",
            data =  list(name = name,
                         ror = ror),
            endpoint = endpoint)
}
