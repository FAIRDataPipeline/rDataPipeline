#' Read array component from HDF5 file
#'
#' Function to read array type data from hdf5 file.
#'
#' @param handle an object of class \code{fdp, R6} containing metadata required
#' by the Data Pipeline API
#' @param data_product a \code{string} specifying a data product
#' @param component a \code{string} specifying a data product component
#'
#' @return Returns an array with attached \code{Dimension_i_title},
#' \code{Dimension_i_units}, \code{Dimension_i_values}, and \code{units}
#' attributes, if available
#'
#' @export
#'
read_array <- function(handle,
                       data_product,
                       component) {

  # If data product is missing from config file throw an error
  check_config(handle, data_product, "read")

  # Read file ---------------------------------------------------------------

  tmp <- resolve_read(handle, data_product, component)
  read_dataproduct <- tmp$data_product
  read_component <- tmp$component
  read_component_url <- tmp$component_url
  read_version <- tmp$version
  read_namespace <- tmp$namespace
  path <- tmp$path

  if (!file.exists(path)) usethis::ui_stop("File missing from data store")

  file.h5 <- rhdf5::h5read(path, read_component)

  # Extract data object
  object <- file.h5$array
  if (is.vector(object)) object <- t(matrix(object))

  # Extract dimension names and make sure they're in the right order
  ind <- grep("Dimension_[0-9]*_names", names(file.h5))
  h5file <- file.h5[ind]
  ord <- order(names(h5file))
  h5file <- h5file[ord]

  # Attach dimension names to the object
  for (i in seq_along(h5file)) {
    if (i == 1) {
      rownames(object) <- h5file[[i]]
    } else if (i == 2) {
      colnames(object) <- h5file[[i]]
    } else {
      dimnames(object)[[i]] <- h5file[[i]]
    }
  }

  # Attach remaining list elements as attributes
  ind <- grep("Dimension_[0-9]_names|array", names(file.h5))
  h5attr <- file.h5[-ind]

  for (i in seq_along(h5attr)) {
    attr(object, names(h5attr)[i]) <- h5attr[[i]]
  }

  rhdf5::h5closeAll()

  # Write to handle ---------------------------------------------------------

  # If data product is already recorded in handle return index
  index <- check_handle(handle, data_product, "inputs", component)

  if (is.null(index))
    handle$input(data_product = data_product,
                 use_data_product = read_dataproduct,
                 use_component = read_component,
                 use_version = read_version,
                 use_namespace = read_namespace,
                 path = path,
                 component_url = read_component_url)

  cli::cli_alert_success(
    "Reading {.value {read_component}} from {.value {read_dataproduct}}")

  # Generate output ---------------------------------------------------------

  object
}
