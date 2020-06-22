#' new_source
#'
#' @param name name of the source
#' @param description free text description of the source
#' @param responsible_person reference to the responsible_person
#' @param store reference to the storage_location
#' @param source_type reference to the source_type
#'
#' @export
#'
new_source <- function(name,
                       description,
                       responsible_person,
                       store,
                       source_type) {

  list(name = name,
       description = description,
       responsible_person = responsible_person,
       store = store,
       source_type = source_type)
}
