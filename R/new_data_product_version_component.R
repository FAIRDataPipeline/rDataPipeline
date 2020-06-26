#' new_data_product_version_component
#'
#' @param name
#' @param responsible_person Name of the responsible person
#' @param data_product_version Name of data product version
#' @param model_runs
#' @param key GitHub key
#'
#' @export
#'
new_data_product_version_component <- function(name,
                                               responsible_person,
                                               data_product_version,
                                               model_runs,
                                               key) {

  rp_url <- get_responsible_person(responsible_person, key)
  # dpv_url <- get_url("data_product_version", list(name = data_product_version))

  post_data(table = "data_product_version_component",
            data =  list(name = name,
                         responsible_person = rp_url,
                         data_product_version = data_product_version,
                         model_runs = model_runs),
            key)
}
