#' check_yaml_write
#' 
#' @keywords internal
#'
#' @param handle \code{fdp} object
#' @param data_product a \code{string} specifying the name of the data product
#'
check_yaml_write <- function(handle, data_product) {

  endpoint <- handle$yaml$run_metadata$local_data_registry_url

  # Check that `write:` section is listed in config yaml
  if (!"write" %in% names(handle$yaml))
    usethis::ui_stop(paste(usethis::ui_field("write"),
                           "section not listed in config.yaml"))

  # Check data product name isn't too long
  tmp <- get_fields("data_product",
                    endpoint = endpoint)
  index <- which(tmp$field == "name")
  data_product_fields <- tmp[index, ]
  max_length_name <- data_product_fields$max_length
  if (nchar(data_product) > max_length_name)
    usethis::ui_stop(paste(usethis::ui_field(data_product),
                           "must be",
                           max_length_name, "characters or less"))
}
