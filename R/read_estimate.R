#' Read estimate component from TOML file
#'
#' Function to read point-estimate type data from toml file.
#'
#' @param handle an object of class \code{fdp, R6} containing metadata required
#' by the Data Pipeline API
#' @param data_product a \code{string} specifying a data product
#' @param component a \code{string} specifying a data product component
#'
#' @export
#'
read_estimate <- function(handle,
                          data_product,
                          component) {

  # If data product is missing from config file throw an error
  check_config(handle, data_product, "read")

  # Read file ---------------------------------------------------------------

  tmp <- resolve_read(handle, data_product, component)
  read_data_product <- tmp$data_product
  read_component <- tmp$component
  path <- tmp$path

  if (!file.exists(path)) usethis::ui_stop("File missing from data store")

  contents <- configr::read.config(file = path)
  this_component <- contents[[read_component]]

  # Check file --------------------------------------------------------------

  if (this_component$type != "point-estimate") {
    msg <- "The file you are trying to read does not contain a point-estimate"
    usethis::ui_stop(msg)
  }

  # Write to handle ---------------------------------------------------------

  # If data product is already recorded in handle return index
  index <- check_handle(handle, data_product, "inputs", component)

  if (is.null(index))
    handle$input(data_product = data_product,
                 use_data_product = read_data_product,
                 use_component = read_component,
                 use_version = tmp$version,
                 use_namespace = tmp$namespace,
                 path = path,
                 component_url = tmp$component_url)

  cli::cli_alert_success(
    "Reading {.value {read_component}} from {.value {read_data_product}}")

  # Generate output ---------------------------------------------------------

  this_component$value
}
