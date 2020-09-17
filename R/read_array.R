#' read_array
#'
#' @param filename a \code{string} specifying the filename of the file to be
#' read
#' @param path a \code{string} specifying the directory of the file to be
#' read
#' @param component a \code{string} specifying a location within the hdf5 file,
#' e.g. "location/per_week/all_deaths"
#'
#' @return Returns an array with attached \code{Dimension_i_title},
#' \code{Dimension_i_units}, \code{Dimension_i_values}, and \code{units}
#' attributes, if available
#'
#' @export
#'
#' @examples
#' df <- data.frame(a = 1:2, b = 3:4)
#' rownames(df) <- 1:2
#' array <- as.matrix(df)
#'
#' create_array(filename = "test_array.h5",
#'              path = ".",
#'              component = "level/a/s/d/f/s",
#'              array = array,
#'              dimension_names = list(rowvalue = rownames(df),
#'                                     colvalue = colnames(df)),
#'              dimension_values = list(NA, 10),
#'              dimension_units = list(NA, "km"),
#'              units = "s")
#'
#' read_array(filename = "test_array.h5",
#'            path = ".",
#'            component = "level/a/s/d/f/s")
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
