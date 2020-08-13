#' new_object_component
#'
#' Upload information to the `object_component` table in the data registry
#'
#' @param name a `string` specifying the name of the `object_component`,
#' unique in the context of `object_component` and its `object` reference
#' *e.g.* "age_group/week-persons-country-all_deaths"
#' @param object_id a `string` specifying the API URL of the
#' associated `object` table *e.g.* "https://data.scrc.uk/api/object/156/"
#' @param description (optional) a `string` containing a free text
#' description of the `object_component`
#' @param key API token from data.scrc.uk
#'
#' Note that the `object_component` table contains `issues` is an
#' additional optional field. This is not included here. Instead use
#' `attach_issue()` and associated functionality to attach issues to
#' objects and objet components.
#'
#' @export
#'
new_object_component <- function(name,
                                 object_id,
                                 description = "",
                                 key) {

  post_data(table = "object_component",
            data = list(name = name,
                        object = object_id,
                        description = description),
            key)

}
