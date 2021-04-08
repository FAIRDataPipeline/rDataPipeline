#' Post entry to object table
#'
#' Upload information to the \code{object} table in the data registry
#'
#' @param storage_location_id a \code{string} specifying the API URL of the
#' associated \code{storage_location} table
#' *e.g.* "https://data.scrc.uk/api/storage_location/411/"
#' @param description (optional) a \code{string} containing a free text
#' description of the \code{object_component}
#'
#' Note that the \code{object} table contains \code{issues} as an
#' additional optional field. This is not included here. Instead use
#' \code{attach_issue()} and associated functionality to attach issues to
#' objects and object components.
#'
#' @family new functions
#'
#' @export
#'
new_object <- function(storage_location_id,
                       description = "") {

  post_data(table = "object",
            data = list(description = description,
                        storage_location = storage_location_id))
}
