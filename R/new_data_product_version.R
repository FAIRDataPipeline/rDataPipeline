#' new_data_product_version
#'
#' @param version_identifier Version identifier of the data_product_version, e.g. 1
#' @param description Free text description of the source_version
#' @param responsible_person Name of the responsible person
#' @param supercedes Superseded source_version
#' @param data_product Name of the data product
#' @param store Name of the storage_location, e.g. "Model File"
#' @param accessibility Name of accessibility type
#' @param processing_script_version
#' @param source_versions
#' @param components
#' @param model_runs
#'
#' @export
#'
new_data_product_version <- function(version_identifier,
                                     description,
                                     responsible_person,
                                     supercedes,
                                     data_product,
                                     store,
                                     accessibility,
                                     processing_script_version,
                                     source_versions,
                                     components,
                                     model_runs,
                                     key) {

  rp_url <- get_responsible_person(responsible_person, key)
  dp_url <- get_url("data_product", list(name = data_product))
  location_url <- get_url("storage_location", list(name = store))
  accessibility_url <- get_url("accessibility", list(name = accessibility))
  script_url <- get_url("processing_script_version",
                        list(name = processing_script_version))

  post_data(table = "data_product_version",
            data =  list(version_identifier = version_identifier,
                         description = description,
                         responsible_person = rp_url,
                         supercedes = supercedes,
                         data_product = dp_url,
                         store = location_url,
                         accessibility = accessibility_url,
                         processing_script_version = script_url,
                         source_versions = source_versions,
                         components = components,
                         model_runs = model_runs),
            key)
}
