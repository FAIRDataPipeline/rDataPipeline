#' Link path to external format data
#'
#' @param handle an object of class \code{fdp, R6} containing metadata required
#' by the Data Pipeline API
#' @param data_product a \code{string} representing an external object in the
#' config.yaml file
#'
#' @return Returns a \code{string} specifying the location of the data product
#' to be read
#'
#' @export
#'
link_read <- function(handle, data_product) {

  # If data product is already recorded in handle return path
  path <- check_handle(handle, data_product, "inputs")
  if (!is.null(path)) return(path)

  # If data product is missing from config file throw an error
  check_config(handle, data_product, "read")

  # Get metadata ------------------------------------------------------------

  tmp <- resolve_read(handle = handle,
                      data_product = data_product)
  read_dataproduct <- tmp$data_product
  read_component <- tmp$component
  read_component_url <- tmp$component_url
  read_version <- tmp$version
  read_namespace <- tmp$namespace
  read_path <- tmp$path

  usethis::ui_info(paste0("Locating ", usethis::ui_value(data_product)))

  # Write to handle ---------------------------------------------------------

  handle$input(data_product = data_product,
               use_data_product = read_dataproduct,
               use_component = read_component,
               use_version = read_version,
               use_namespace = read_namespace,
               path = read_path,
               component_url = read_component_url)

  # Return storage location -------------------------------------------------

  invisible(read_path)
}
