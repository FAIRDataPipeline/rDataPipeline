#' check_yaml_write
#'
#' @param handle \code{fdp} object
#' @param data_product a \code{string} specifying the name of the data product
#'
check_yaml_write <- function(handle, data_product) {
  # Check that `write:` section is listed in config yaml
  if (!"write" %in% names(handle$yaml))
    usethis::ui_stop(paste(usethis::ui_field("write"),
                           "section not listed in config.yaml"))

  # Check that data product name is listed in `write:` section of config yaml
  listed <- lapply(handle$yaml$write, function(x) x$data_product) %>% unlist()

  if (!data_product %in% listed)
    usethis::ui_stop(paste(usethis::ui_field(data_product),
                           "not listed in config.yaml"))
}
