#' create_distribution
#'
#' Function to populate hdf5 file with distribution type data.
#'
#' @param h5filename a \code{string} specifying the name of the hdf5 file
#' @param component a \code{string} specifying a location within the hdf5 file
#' @param distribution a \code{string} specifying the name of the distribution
#' @param parameter_values a \code{numeric} \code{vector} specifying the
#' parameter values
#' @param parameter_names a \code{vector} specifying the name of each parameter
#' value
#'
#' @export
#'
create_distribution <- function(h5filename,
                                component,
                                distribution,
                                parameter_values,
                                parameter_names) {

  # Generate hdf5 structure
  file.h5 <- H5File$new(h5filename)

  directory.structure <- strsplit(component, "/")[[1]]
  levels <- length(directory.structure)

  tmp.path <- ""
  tmp.groups <- names(file.h5)

  for (i in seq_along(directory.structure)) {
    if(!directory.structure[i] %in% tmp.groups)
      file.h5$create_group(file.path(tmp.path, directory.structure[i]))

    tmp.path <- file.path(tmp.path, directory.structure[i])
    tmp.groups <- names(file.h5[[tmp.path]])
  }

  # Attach data
  file.h5[[file.path(component, "distribution")]] <- distribution
  file.h5[[file.path(component, "parameter_values")]] <- parameter_values
  file.h5[[file.path(component, "parameter_names")]] <- parameter_names
}
