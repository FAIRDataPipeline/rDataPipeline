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

  file.h5 <- rhdf5::h5read(file.path(path, filename), component)
  object <- file.h5$array

  if(any("row_names" %in% names(file.h5)))
    rownames(object) <- file.h5$row_names

  rhdf5::h5closeAll()
  object
}
