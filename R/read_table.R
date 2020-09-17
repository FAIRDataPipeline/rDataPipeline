#' read_table
#'
#' @param filename a \code{string} specifying the filename, e.g. "0.1.0.h5"
#' @param path a \code{string} specifying the directory in which you want to save the h5 file
#' @param component a \code{string} specifying a location within the hdf5 file,
#' e.g. "location/per_week/all_deaths"
#'
#' @export
#'
read_table <- function(filename,
                       path,
                       component) {
  # Read hdf5 file
  file.h5 <- rhdf5::h5read(file.path(path, filename), component)

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
  object
}
