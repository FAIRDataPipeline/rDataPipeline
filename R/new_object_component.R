#' Post entry to object_component table
#'
#' Upload information to the \code{object_component} table in the data registry
#'
#' @param name a \code{string} specifying the name of the
#' \code{object_component}, unique in the context of \code{object_component}
#' and its \code{object} reference
#' @param object_id a \code{string} specifying the API URL of the
#' associated \code{object} table
#' @param description (optional) a \code{string} containing a free text
#' description of the \code{object_component}
#'
#' Note that the \code{object_component} table contains \code{issues} as an
#' additional optional field. This is not included here. Instead use
#' \code{attach_issue()} and associated functionality to attach issues to
#' objects and objet components.
#'
#' @family new functions
#'
#' @export
#'
#' @examples
#' \dontrun{
#' \donttest{
#' new_object_component(name = "age_group/week-persons-country-all_deaths",
#'                      object_id = "https://data.scrc.uk/api/object/156/",
#'                      description = "A very useful component",
#'                      key = key)
#' }}
#'
new_object_component <- function(name,
                                 object_id,
                                 description = "") {

  post_data(table = "object_component",
            data = list(name = name,
                        object = object_id,
                        description = description))

}
