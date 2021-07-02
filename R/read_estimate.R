#' Read estimate from TOML file
#'
#' Function to read point-estimate type data from toml file.
#'
#' @param handle \code{fdp} object
#' @param data_product a \code{string} specifying a data product
#' @param component a \code{string} specifying a data product component
#'
#' @export
#'
read_estimate <- function(handle,
                          data_product,
                          component) {

  # Read file ---------------------------------------------------------------

  tmp <- resolve_read(handle, data_product, component)
  read_data_product <- tmp$data_product
  read_component <- tmp$component
  read_component_url <- tmp$component_url
  read_version <- tmp$version
  read_namespace <- tmp$namespace
  path <- tmp$path

  contents <- configr::read.config(file = path)
  this_component <- contents[[read_component]]

  # Check file --------------------------------------------------------------

  if (this_component$type != "point-estimate") {
    msg <- "The file you are trying to read does not contain a point-estimate"
    usethis::ui_stop(msg)
  }

  # Write to handle ---------------------------------------------------------

  handle$input(data_product = data_product,
               use_data_product = read_data_product,
               use_component = read_component,
               use_version = read_version,
               use_namespace = read_namespace,
               path = path,
               component_url = read_component_url)

  cli::cli_alert_success(
    "Reading {.value {read_component}} from {.value {read_data_product}}")

  # Generate output ---------------------------------------------------------

  this_component$value
}
