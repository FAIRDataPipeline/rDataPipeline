#' read_array
#'
#' @param filename a \code{string} specifying the filename, e.g. "0.1.0.h5"
#' @param path a \code{string} specifying the directory in which you want to save the h5 file
#' @param component a \code{string} specifying a location within the hdf5 file,
#' e.g. "location/per_week/all_deaths"
#'
#' @export
#'
read_array <- function(filename,
                       path,
                       component) {

  file.h5 <- rhdf5::h5read(file.path(path, filename), component)
  object <- file.h5$array
  if(is.vector(object)) object <- t(matrix(object))

  object <- as.data.frame(object)
  rownames(object) <- file.h5$Dimension_1_names
  colnames(object) <- file.h5$Dimension_2_names

  rhdf5::h5closeAll()
  object
}
