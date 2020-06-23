#' new_source
#'
#' @param name Name of the source
#' @param description Free text description of the source
#' @param responsible_person Name of the responsible person
#' @param store Name of the storage_location, e.g. "Model File"
#' @param source_type Name of source type
#' @param key GitHub key
#'
#' @export
#'
new_source <- function(name,
                       description,
                       responsible_person,
                       store,
                       source_type,
                       key) {

  rp_url <- get_responsible_person(responsible_person, key)
  store_url <- get_url(table = "storage_location", key = store)
  type_url <- get_url(table = "source_type", key = source_type)

  post_data(
    table = "source",
    data =  list(name = name,
                 description = description,
                 responsible_person = rp_url,
                 store = store_url,
                 source_type = type_url),
    key)
}
