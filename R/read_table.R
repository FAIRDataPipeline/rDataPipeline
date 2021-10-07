#' Read table component from HDF5 file
#'
#' Function to read table type data from hdf5 file.
#'
#' @param handle an object of class \code{fdp, R6} containing metadata required
#' by the Data Pipeline API
#' @param data_product a \code{string} specifying a data product
#' @param component a \code{string} specifying a data product component
#'
#' @return Returns a \code{data.frame} with attached \code{column_units}
#' attributes, if available
#'
#' @export
#'
read_table <- function(handle,
                       data_product,
                       component) {

  # If data product is missing from config file throw an error
  check_config(handle, data_product, "read")

  # Read file ---------------------------------------------------------------

  tmp <- resolve_read(handle = handle,
                      data_product = data_product,
                      component = component)
  read_dataproduct <- tmp$data_product
  read_component <- tmp$component
  read_component_url <- tmp$component_url
  read_version <- tmp$version
  read_namespace <- tmp$namespace
  path <- tmp$path

  if (!file.exists(path)) usethis::ui_stop("File missing from data store")

  file.h5 <- rhdf5::h5read(path, read_component)

  # Extract data object
  object <- file.h5$table

  # Attach rownames to object
  if (any("row_names" %in% names(file.h5)))
    rownames(object) <- file.h5$row_names

  # Attach remaining list elements as attributes
  ind <- grep("row_names|table", names(file.h5))
  tmp <- file.h5[-ind]

  for (i in seq_along(tmp)) {
    attr(object, names(tmp)[i]) <- tmp[[i]]
  }

  rhdf5::h5closeAll()

  object <- data.frame(lapply(object, type.convert, as.is = TRUE))

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
