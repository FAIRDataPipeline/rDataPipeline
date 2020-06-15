#' create_number
#'
#' Function to populate hdf5 file with number type data.
#'
#' @param h5filename a \code{string} specifying the name of the hdf5 file
#' @param component a \code{string} specifying a location within the hdf5 file
#' @param number a \code{numeric} object specifying the value
#' @param name a \code{string} descriptor
#'
#' @export
#'
create_number <- function(h5filename,
                          component,
                          number,
                          name) {

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
  file.h5[[file.path(component, "number")]] <- number
  file.h5[[file.path(component, "name")]] <- name
}
