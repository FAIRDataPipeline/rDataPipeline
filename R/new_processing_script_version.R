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
  script_url <- get_url("processing_script", list(name = processing_script))
  location_url <- get_url("storage_location", list(name = store))
  accessibility_url <- get_url("accessibility", list(name = accessibility))

  # Get data_product_versions
  # dp_url <- get_url("data_product", list(name = data_product))
  # ind <- lapply(get_existing("data_product_version"), function(x)
  #   x$data_product == dp_url) %>% unlist() %>% which()
  # data_product_versions <- lapply(ind, function(x)
  #   get_existing("data_product_version")[[x]]$url)

  post_data(table = "processing_script_version",
            data = list(version_identifier = version_identifier,
                        responsible_person = rp_url,
                        supercedes = supercedes,
                        processing_script = script_url,
                        store = location_url,
                        accessibility = accessibility_url,
                        data_product_versions = data_product_versions),
            key)

}
