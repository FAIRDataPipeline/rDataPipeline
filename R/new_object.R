#' Post entry to object table
#'
#' Upload information to the \code{object} table in the data registry
#'
#' @param storage_location_uri a \code{string} specifying the URI of an entry
#' in the \code{storage_location} table
#' @param description (optional) a \code{string} containing a free text
#' description of the \code{object}
#' @param issues a \code{list} of Issues URLs to associate with this Object
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
new_object <- function(storage_location_uri,
                       description = "",
                       issues) {
  if (missing(issues)) {
    post_data(table = "object",
              data = list(description = description,
                          storage_location = storage_location_uri))
  } else {
    post_data(table = "object",
              data = list(description = description,
                          storage_location = storage_location_uri,
                          issues = issues))
  }
}
