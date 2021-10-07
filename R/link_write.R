#' Link path for external format data
#'
#' @param handle an object of class \code{fdp, R6} containing metadata required
#' by the Data Pipeline API
#' @param data_product a \code{string} representing an external object in the
#' config.yaml file
#'
#' @return Returns a \code{string} specifying the location in which the data
#' product should be written
#'
#' @export
#'
link_write <- function(handle, data_product) {

  # If data product is already recorded in handle return path
  path <- check_handle(handle, data_product, "outputs")
  if (!is.null(path)) return(path)

  # If data product is missing from config file throw an error
  check_config(handle, data_product, "write")

  # Get metadata ------------------------------------------------------------

  write_metadata <- resolve_write(handle = handle,
                                  data_product = data_product)
  path <- write_metadata$path

  # Generate directory structure --------------------------------------------

  directory <- dirname(path)
  if (!file.exists(directory)) dir.create(directory, recursive = TRUE)

  # Write to handle ---------------------------------------------------------

  handle$output(data_product = data_product,
                use_data_product = write_metadata$data_product,
                use_component = NA,
                use_version = write_metadata$version,
                use_namespace = write_metadata$namespace,
                path = path,
                data_product_description = write_metadata$description,
                component_description = NA,
                public = write_metadata$public)

  invisible(path)
}
