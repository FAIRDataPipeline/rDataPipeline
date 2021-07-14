#' Post entry to organisation table
#'
#' Upload information to the \code{organisation} table in the data registry
#'
#' @param name a \code{string} specifying the name of the \code{organisation}
#' @param identifier (optional) a \code{string} representing
#' the organisation identifier URL, *e.g.* ROR ID
#' @param endpoint a \code{string} specifying the registry endpoint
#'
#' @family new functions
#'
#' @export
#'
new_organisation <- function(name,
                             identifier,
                             endpoint = "http://localhost:8000/api/") {

  data <- list(name = name)

  if (!missing(identifier))
    data$identifier <- identifier

  post_data(table = "organisation",
            data =  data,
            endpoint = endpoint)
}
