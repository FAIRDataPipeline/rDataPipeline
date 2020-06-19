#' create_array
#'
#' Function to populate hdf5 file with array type data.
#'
#' @param h5filename a \code{string} specifying the name of the hdf5 file
#' @param component a \code{string} specifying a location within the hdf5 file
#' @param array a \code{matrix} containing the data
#' @param dimension_names a \code{list} where each element is a vector
#' containing the labels associated with a particular dimension (e.g.
#' element 1 corresponds to dimension 1, which corresponds to row names) and
#' the name of each element describes the contents of each dimension (e.g. age
#' classes).
#' @param dimension_values (optional) a \code{list} of values corresponding to
#' each dimension (e.g. list element 2 corresponds to columns)
#' @param dimension_units (optional) a \code{list} of units corresponding to
#' each dimension (e.g. list element 2 corresponds to columns)
#' @param units (optional) a \code{string} specifying the units of the data as
#' a whole
#'
#' @export
#'
create_array <- function(h5filename,
                         component,
                         array,
                         dimension_names,
                         dimension_values,
                         dimension_units,
                         units) {

  if(!grepl(".h5$", h5filename)) stop("h5filename must be *.h5")
  if(!is.numeric(array)) stop("array must be a numeric")
  if(!is.matrix(array)) stop("array must be a matrix")
  if(!is.vector(dimension_names[[1]])) stop("Dimension_1_names must be a vector")
  if(!is.vector(dimension_names[[2]])) stop("Dimension_2_names must be a vector")
  if(length(dimension_names[[1]]) != nrow(array))
    stop("Dimension_1_names length must match nrows in array")
  if(length(dimension_names[[2]]) != ncol(array))
    stop("Dimension_2_names length must match nrows in array")

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
  file.h5[[file.path(component, "array")]] <- array

  dimension_titles <- names(dimension_names)

  # Attach row attributes
  file.h5[[file.path(component, "Dimension_1_title")]] <- dimension_titles[1]
  file.h5[[file.path(component, "Dimension_1_names")]] <- dimension_names[[1]]

  # Attach column atttributes
  file.h5[[file.path(component, "Dimension_2_title")]] <- dimension_titles[2]
  file.h5[[file.path(component, "Dimension_2_names")]] <- dimension_names[[2]]

  # Attach additional attributes
  if(!missing(dimension_values)) {
    dimensions.with.values <- which(!is.na(dimension_values))

    for(i in dimensions.with.values)
      eval(parse(text = paste0("file.h5[[file.path(component, \"Dimension_", i,
                               "_values\")]] <- dimension_values[[i]]")))
  }

  if(!missing(dimension_units)) {
    dimensions.with.units <- which(!is.na(dimension_units))

    for(i in dimensions.with.units)
      eval(parse(text = paste0("file.h5[[file.path(component, \"Dimension_", i,
                               "_units\")]] <- dimension_units[[i]]")))
  }

  if(!missing(units))
    eval(parse(text = paste0("file.h5[[file.path(component, \"units\")]] <- units")))

  file.h5$close_all()
}


