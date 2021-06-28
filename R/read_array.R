#' Read component from array-type H5 file
#'
#' Function to read array type data from hdf5 file.
#'
#' @param handle \code{fdp} object
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

  # Read hdf5 file
  path <- resolve_read(handle, data_product)
  datastore <- handle$yaml$run_metadata$write_data_store
  file.h5 <- rhdf5::h5read(paste0(datastore, path), component)

  # Extract data object
  object <- file.h5$array
  if(is.vector(object)) object <- t(matrix(object))

  # Extract dimension names and make sure they're in the right order
  ind <- grep("Dimension_[0-9]*_names", names(file.h5))
  tmp <- file.h5[ind]
  ord <- order(names(tmp))
  tmp <- tmp[ord]

  # Attach dimension names to the object
  for(i in seq_along(tmp)) {
    if(i == 1) {
      rownames(object) <- tmp[[i]]
    } else if(i == 2) {
      colnames(object) <- tmp[[i]]
    } else {
      dimnames(object)[[i]] <- tmp[[i]]
    }
  }

  # Attach remaining list elements as attributes
  ind <- grep("Dimension_[0-9]_names|array", names(file.h5))
  tmp <- file.h5[-ind]

  for(i in seq_along(tmp)) {
    attr(object, names(tmp)[i]) <- tmp[[i]]
  }

  rhdf5::h5closeAll()
  object
}
