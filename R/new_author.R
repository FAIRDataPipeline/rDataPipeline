#' Post entry to author table
#'
#' Upload information to the \code{author} table in the data registry
#'
#' @param family_name a \code{string} specifying the author's family name
#' @param given_name a \code{string} specifying the author's first name
#' and / or middle name and / or any initials
#' @param identifier (optional) a \code{string} specifying the full URL of
#' the \code{author}s identifier, *e.g.* ORCID iD
#'
#' @family new functions
#'
#' @export
#'
new_author <- function(family_name,
                       given_name,
                       identifier) {

  data <- list(family_name = family_name,
               given_name = given_name)

  if (!missing(identifier))
    data$identifier <- identifier

  post_data(table = "author",
            data =  data)
}
