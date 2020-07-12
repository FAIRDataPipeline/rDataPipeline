#' read_table
#'
#' @param filename a \code{string} specifying the filename, e.g. "0.1.0.h5"
#' @param component a \code{string} specifying a location within the hdf5 file,
#' e.g. "location/per_week/all_deaths"
#'
#' @export
#'
read_table <- function(filename, component) {
  file.h5 <- H5File$new(filename, mode = "r")

  object <- file.h5[[paste0(component, "/table")]][]
  if(any("row_names" %in% names(file.h5[[component]])))
    rownames(object) <- file.h5[[paste0(component, "/row_names")]][]

  file.h5$close_all()
  object
}
