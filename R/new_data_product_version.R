#' new_data_product_version
#'
#' @param version_identifier
#' @param description
#' @param responsible_person Name of the responsible person
#' @param supercedes Superseded source_version
#' @param data_product
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

  post_data(table = "data_product_version",
            data =  list(version_identifier = version_identifier,
                         description = description,
                         responsible_person = responsible_person,
                         supercedes = supercedes,
                         data_product = data_product,
                         store = store,
                         accessibility = accessibility,
                         processing_script_version = processing_script_version,
                         source_versions = source_versions,
                         components = components,
                         model_runs = model_runs),
            key)
}
