#' new_data_product_type
#'
#' @param name Name of the data product type
#' @param description Free text decription of the data product type
#' @param key GitHub key
#'
#' @export
#'
new_data_product_type <- function(name,
                                  description, key) {

  post_data(table = "data_product_type",
            data =  list(name = name,
                         description = description),
            key)
}
