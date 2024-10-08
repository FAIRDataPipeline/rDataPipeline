#' Post entry to author table
#'
#' Upload information to the \code{author} table in the data registry
#' 
#' @keywords internal
#'
#' @param name a \code{string} specifying the full name or organisation name of
#' the \code{author}; note that at least one of name or identifier must be
#' specified
#' @param identifier (optional) a \code{string} specifying the full URL
#' identifier (*e.g.* ORCiD or ROR ID) of the \code{author}
#' @param endpoint a \code{string} specifying the registry endpoint
#'
#' @family new functions
#'
new_author <- function(name,
                       identifier,
                       endpoint = "http://127.0.0.1:8000/api/") {

  data <- list()

  if (!missing(name))
    data$name <- name

  if (!missing(identifier))
    data$identifier <- identifier

  post_data(table = "author",
            data =  data,
            endpoint = endpoint)
}
