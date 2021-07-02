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

  # Read file
  tmp <- resolve_read(handle, data_product)
  path <- tmp$path
  contents <- configr::read.config(file = path)
  this_component <- contents[[component]]

  # Check file
  if (this_component$type != "point-estimate") {
    msg <- "The file you are trying to read does not contain a point-estimate"
    usethis::ui_stop(msg)
  }

  this_component$value
}
