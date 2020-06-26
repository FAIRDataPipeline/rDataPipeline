#' new_data_product
#'
#' @param name Name of the data product
#' @param description Free text description of the data product
#' @param responsible_person Name of the responsible person
#' @param type Name of data product type
#' @param versions
#' @param key GitHub key
#'
#' @export
#'
new_data_product <- function(name,
                             description,
                             responsible_person,
                             type,
                             versions,
                             key) {

  rp_url <- get_responsible_person(responsible_person, key)
  type_url <- get_url("data_product_type", list(name = type))
  versions_url <- lapply(versions, function(x)
    get_url("data_product_version_component", list(name = x)))

  post_data(table = "data_product",
            data =  list(name = name,
                         description = description,
                         responsible_person = rp_url,
                         type = type_url,
                         versions = versions),
            key)
}
