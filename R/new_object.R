#' new_object
#'
#' Upload information to the `object` table in the data registry
#'
#' @param storage_location_id a `string` specifying the API URL of the
#' associated `storage_location` table
#' *e.g.* "https://data.scrc.uk/api/storage_location/411/"
#' @param description (optional) a `string` containing a free text
#' description of the `object_component`
#' @param key API token from data.scrc.uk
#'
#' Note that the `object` table contains `issues` is an
#' additional optional field. This is not included here. Instead use
#' `attach_issue()` and associated functionality to attach issues to
#' objects and object components.
#'
#' @export
#'
new_object <- function(storage_location_id,
                       description = "",
                       key) {

  post_data(table = "object",
            data = list(description = description,
                        storage_location = storage_location_id),
            key)
}
