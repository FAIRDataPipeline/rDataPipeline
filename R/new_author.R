#' Post entry to author table
#'
#' Upload information to the \code{author} table in the data registry
#'
#' @param family_name a \code{string} specifying the author's family name
#' @param given_name a \code{string} specifying the author's first name
#' and / or middle name and / or any initials
#' @param identifier (optional) a \code{string} specifying the full URL of
#' the \code{author}s identifier, *e.g.* ORCID iD
#' @param endpoint a \code{string} specifying the registry endpoint
#'
#' @family new functions
#'
new_author <- function(family_name,
                       given_name,
                       identifier,
                       endpoint = "http://localhost:8000/api/") {

  data <- list(family_name = family_name,
               given_name = given_name)

  if (!missing(identifier))
    data$identifier <- identifier

  post_data(table = "author",
            data =  data,
            endpoint = endpoint)
}
