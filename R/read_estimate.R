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
  path <- resolve_read(handle, data_product)
  datastore <- handle$yaml$run_metadata$write_data_store
  fullpath <- paste0(datastore, path)
  contents <- configr::read.config(file = fullpath)

  # Check file
  if (contents[[1]]$type != "point-estimate") {
    msg <- "The file you are trying to read does not contain a point-estimate"
    usethis::ui_stop(msg)
  }

  contents
}
