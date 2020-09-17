#' read_array
#'
#' @param filename a \code{string} specifying the filename, e.g. "0.1.0.h5"
#' @param path a \code{string} specifying the directory in which you want to
#' save the h5 file
#' @param component a \code{string} specifying a location within the hdf5 file,
#' e.g. "location/per_week/all_deaths"
#'
#' @return Returns an array
#'
#' @export
#'
read_array <- function(filename,
                       path,
                       component) {
  # Read hdf5 file
  file.h5 <- rhdf5::h5read(file.path(path, filename), component)

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
