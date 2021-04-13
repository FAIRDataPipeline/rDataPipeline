#' Create array-type H5 file
#'
#' Function to populate hdf5 file with array type data.
#'
#' @param array an \code{array} containing the data
#' @param handle list
#' @param data_product a \code{string} specifying the name of the data product
#' @param component a \code{string} specifying a location within the hdf5 file
#' @param dimension_names a \code{list} where each element is a vector
#' containing the labels associated with a particular dimension (e.g.
#' element 1 corresponds to dimension 1, which corresponds to row names) and
#' the name of each element describes the contents of each dimension (e.g. age
#' classes).
#' @param dimension_values (optional) a \code{list} of values corresponding to
#' each dimension (e.g. list element 2 corresponds to columns)
#' @param dimension_units (optional) a \code{list} of units corresponding to
#' each dimension (e.g. list element 2 corresponds to columns)
#' @param units (optional) a \code{string} specifying the units of the data as
#' a whole
#'
#' @family write functions
#'
#' @export
#'
write_array <- function(array,
                        handle,
                        data_product,
                        component,
                        dimension_names,
                        dimension_values,
                        dimension_units,
                        units) {

  # Extract metadata from config.yaml
  datastore <- handle$yaml$run_metadata$default_data_store
  namespace <- handle$yaml$run_metadata$default_output_namespace
  dataproduct <- handle$yaml$write[[1]]$data_product

  # Find / set save location
  existing_outputs <- handle$outputs
  if (dataproduct %in% names(existing_outputs)) {
    save_to <- unname(existing_outputs[dataproduct])
  } else {
    filename <- paste0(openssl::sha1(as.character(Sys.time())), ".h5")
    save_to <- file.path(datastore, namespace, dataproduct, filename)
    handle$outputs[dataproduct] <- save_to
  }

  # Write component to hdf5 file
  create_array(array,
               save_to,
               data_product,
               component,
               dimension_names,
               dimension_values,
               dimension_units,
               units)

  invisible(handle)
}
