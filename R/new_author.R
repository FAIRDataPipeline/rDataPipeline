#' Post entry to author table
#'
#' Upload information to the \code{author} table in the data registry
#'
#' @param family_name a \code{string} specifying the author's family name
#' @param personal_name a \code{string} specifying the author's first name
#' and / or middle name and / or any initials
#' @param orcid (optional) a \code{string} specifying the ORCID iD of the
#' \code{author}
#'
#' @family new functions
#'
#' @export
#'
new_author <- function(family_name,
                       personal_name,
                       orcid) {

  data <- list(family_name = family_name,
               personal_name = personal_name)

  if (!missing(orcid))
    data$orcid <- orcid

  post_data(table = "author",
            data =  data)
}
