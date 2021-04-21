#' Read component from table-type H5 file
#'
#' Function to read table type data from hdf5 file.
#'
#' @param filepath a \code{string} specifying the path and filename of the file to
#' be read
#' @param component a \code{string} specifying a location within the hdf5 file
#'
#' @return Returns a \code{data.frame} with attached \code{column_units}
#' attributes,
#' if available
#'
#' @export
#'
read_table <- function(filepath,
                       component) {
  # Read hdf5 file
  file.h5 <- rhdf5::h5read(filepath, component)

  # Extract data object
  object <- file.h5$table

  # Attach rownames to object
  if(any("row_names" %in% names(file.h5)))
    rownames(object) <- file.h5$row_names

  # Attach remaining list elements as attributes
  ind <- grep("row_names|table", names(file.h5))
  tmp <- file.h5[-ind]

  for(i in seq_along(tmp)) {
    attr(object, names(tmp)[i]) <- tmp[[i]]
  }

  rhdf5::h5closeAll()
  object <- data.frame(lapply(object, type.convert, as.is = TRUE))
  object
}
