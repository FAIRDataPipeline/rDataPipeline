#' fair_init
#'
#' @param name a \code{string} specifying the full name or organisation name of
#' the \code{author}; note that at least one of name or identifier must be
#' specified
#' @param identifier (optional) a \code{string} specifying the full URL
#' identifier (*e.g.* ORCiD or ROR ID) of the \code{author}
#' @param endpoint a \code{string} specifying the registry endpoint
#'
#' @export
#'
fair_init <- function(name,
                      identifier,
                      endpoint = "http://127.0.0.1:8000/api/") {

  user_url <- get_url(table = "users",
                      query = list(username = "admin"),
                      endpoint = endpoint)

  author_url <- new_author(name = name,
                           identifier = identifier,
                           endpoint = endpoint)

  new_user_author(user_url = user_url,
                  author_url = author_url,
                  endpoint = endpoint)
}
