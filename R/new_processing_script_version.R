#' new_processing_script_version
#'
#' @param version_identifier Version identifier of the source_version, e.g. 1
#' @param responsible_person Name of the responsible person
#' @param supercedes Superseded source_version
#' @param processing_script Name of the processing script
#' @param store Name of the storage_location, e.g. "Model File"
#' @param accessibility Name of accessibility type
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
