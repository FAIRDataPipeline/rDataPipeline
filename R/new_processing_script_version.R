#' new_processing_script_version
#'
#' @param version_identifier
#' @param responsible_person
#' @param supercedes
#' @param processing_script
#' @param store
#' @param accessibility
#' @param data_product_versions
#' @param key GitHub key
#'
#' @export
#'
new_processing_script_version <- function(version_identifier,
                                          responsible_person,
                                          supercedes,
                                          processing_script,
                                          store,
                                          accessibility,
                                          data_product_versions,
                                          key) {

  rp_url <- get_responsible_person(responsible_person, key)
  location_url <- get_url(table = "storage_location", key = store)
  accessibility_url <- get_url(table = "accessibility", key = accessibility)

  post_data(table = "processing_script",
            data = list(version_identifier = version_identifier,
                        responsible_person = responsible_person,
                        supercedes = supercedes,
                        processing_script = processing_script,
                        store = location_url,
                        accessibility = accessibility_url,
                        data_product_versions = data_product_versions),
            key)

}
