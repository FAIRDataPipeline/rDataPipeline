#' Post to author table
#'
#' Upload information to the \code{author} table in the data registry
#'
#' @param family_name a \code{string} specifying the author's family name
#' *e.g.* "Mitchell"
#' @param personal_name a \code{string} specifying the author's first name
#' and / or middle name and / or any initials *e.g.* "Sonia" or "Sonia N" or
#' "Sonia Natalie"
#' @param object_id a \code{string} specifying the API URL of the associated
#' `object` table *e.g.* "https://data.scrc.uk/api/object/31858/"
#' @param key API token from data.scrc.uk
#' @param ... internal parameters
#'
#' @export
#'
new_author <- function(family_name,
                       personal_name,
                       object_id,
                       key, ...) {

  post_data(table = "author",
            data =  list(family_name = family_name,
                         personal_name = personal_name,
                         object = object_id),
            key, ...)
}
