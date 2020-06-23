#' new_source_version
#'
#' @param version_identifier Version identifier of the source_version, e.g. 1
#' @param description Free text description of the source_version
#' @param responsible_person Name of the responsible person
#' @param supercedes Superseded source_version
#' @param source Name of the source
#' @param store Name of the storage_location, e.g. "Model File"
#' @param accessibility Name of accessibility type
#' @param key GitHub key
#'
#' @export
#'
new_source_version <- function(version_identifier,
                               description,
                               responsible_person,
                               supercedes,
                               source,
                               store,
                               accessibility,
                               key) {

  rp_url <- get_responsible_person(responsible_person, key)
  source_url <- get_existing(table = "source", key = source)$url
  store_url <- get_existing(table = "storage_location", key = store)$url
  access_url <- get_existing(table = "accessibility", key = accessibility)$url

  post_data(table = "source_version",
            data =  list(version_identifier = version_identifier,
                         description = description,
                         responsible_person = rp_url,
                         supercedes = supercedes,
                         source = source_url,
                         store = store_url,
                         accessibility = access_url),
            key)
}
