#' read_array
#'
#' @param filename a \code{string} specifying the filename, e.g. "0.1.0.h5"
#' @param component a \code{string} specifying a location within the hdf5 file,
#' e.g. "location/per_week/all_deaths"
#'
#' @export
#'
read_array <- function(filename,
                       component) {

  file.h5 <- H5File$new(filename, mode = "r")
  object <- file.h5[[paste0(component, "/array")]]$read()
  if(is.vector(object)) object <- t(matrix(object))
  #object <- as.data.frame(object)
  colnames(object) <- file.h5[[paste0(component, "/Dimension_2_names")]][]
  rownames(object) <- file.h5[[paste0(component, "/Dimension_1_names")]][]

  file.h5$close_all()
  object
}
