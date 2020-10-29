#' Create array-type H5 file
#'
#' Function to populate hdf5 file with array type data.
#'
#' @param filename a \code{string} specifying the filename, e.g. "0.1.0.h5"
#' @param path a \code{string} specifying the directory in which you want to
#' save the h5 file; this will be automatically generated if it doesn't
#' already exist
#' @param component a \code{string} specifying a location within the hdf5 file,
#' e.g. "location/per_week/all_deaths"
#' @param array an \code{array} containing the data
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
#' @family create functions
#'
#' @export
#'
#' @examples
#' df <- data.frame(a = 1:2, b = 3:4)
#' rownames(df) <- 1:2
#' array <- as.matrix(df)
#'
#' # Create 2-dimensional array
#' create_array(filename = "test_array_2d.h5",
#'              path = ".",
#'              component = "row/column-c1-c2-c3",
#'              array = array,
#'              dimension_names = list(rowvalue = rownames(df),
#'                                     colvalue = colnames(df)))
#' file.remove("test_array_2d.h5")
#'
#' # Create 3-dimensional array
#' create_array(filename = "test_array_3d.h5",
#'              path = ".",
#'              component = "row/column/level-c1",
#'              array = array(c(array, array), dim = c(dim(array), 2)),
#'              dimension_names = list(rowvalue = rownames(df),
#'                                     colvalue = colnames(df),
#'                                     gender = paste0("male", "female")))
#' file.remove("test_array_3d.h5")
#'
#' # Create 4-dimensional array
#' create_array(filename = "test_array_4d.h5",
#'              path = ".",
#'              component = "dim1/dim2/dim3/dim4-c1-c2",
#'              array = array(c(array, array, array), dim = c(dim(array), 2, 2)),
#'              dimension_names = list(rowvalue = rownames(df),
#'                                     colvalue = colnames(df),
#'                                     gender = paste0("male", "female"),
#'                                     city = paste0("glasgow", "paris")))
#' file.remove("test_array_4d.h5")
#'
#' # Create array with values and units
#' create_array(filename = "test_array_val.h5",
#'              path = ".",
#'              component = "row/column-c1-c2",
#'              array = array,
#'              dimension_names = list(rowvalue = rownames(df),
#'                                     colvalue = colnames(df)),
#'              dimension_values = list(NA, 10),
#'              dimension_units = list(NA, "km"),
#'              units = "s")
#' file.remove("test_array_val.h5")
#'
create_array <- function(filename,
                         path = ".",
                         component,
                         array,
                         dimension_names,
                         dimension_values,
                         dimension_units,
                         units) {

  if(!grepl(".h5$", filename)) stop("filename must be *.h5")
  if(!is.array(array)) stop("array must be an array")
  if(length(dimension_names[[1]]) != nrow(array))
    stop("Length of Dimension_1_names must equal the number of rows in array")
  if(length(dimension_names[[2]]) != ncol(array))
    stop("Length of Dimension_2_names must equal the number of columns in array")
  if(length(dim(array)) != length(dimension_names))
    stop("Length of list dimension_names must equal the number of dimensions in array")

  # Generate directory structure
  if(!file.exists(path)) dir.create(path, recursive = TRUE)

  # Generate hdf5 file
  fullname <- file.path(path, filename)
  if(file.exists(fullname)) {
    fid <- H5Fopen(fullname)
    if(length(h5ls(fid)) == 0) {
      current.structure <- ""
    } else {
      current.structure <- gsub("^/", "", unique(h5ls(fid)$group))
    }
    rhdf5::h5closeAll()

  } else {
    fid <- rhdf5::h5createFile(fullname)
    current.structure <- ""
  }

  # Generate internal structure
  if(grepl("/", component)) {
    directory.structure <- strsplit(component, "/")[[1]]
  } else {
    directory.structure <- component
  }

  for (i in seq_along(directory.structure)) {
    # This structure needs to be added
    if(i==1) {
      build.structure <- directory.structure[1]
    } else {
      build.structure <- paste0(build.structure, "/", directory.structure[i])
    }
    # If the structure doesn't exist make it
    if(!build.structure %in% current.structure)
      rhdf5::h5createGroup(fullname, build.structure)
    # Update current structure
    current.structure <- c(current.structure, build.structure)
  }

  # Attach data
  rhdf5::h5write(array, fullname, paste0(component, "/array"))

  # Attach dimension titles
  dimension_titles <- names(dimension_names)

  for(i in seq_along(dimension_titles)) {
    attribute_component <- paste0(component, "/Dimension_", i, "_title")
    rhdf5::h5write(dimension_titles[i], fullname, attribute_component)
  }

  # Attach dimension names
  for(j in seq_along(dimension_names)) {
    attribute_component <- paste0(component, "/Dimension_", j, "_names")
    rhdf5::h5write(dimension_names[[j]], fullname, attribute_component)
  }

  # Attach dimension values
  if(!missing(dimension_values)) {
    dimensions.with.values <- which(!is.na(dimension_values))
    for(k in dimensions.with.values) {
      value_component <- paste0(component, "/Dimension_", k, "_values")
      rhdf5::h5write(dimension_values[[k]], fullname, value_component)
    }
  }

  # Attach dimension units
  if(!missing(dimension_units)) {
    dimensions.with.units <- which(!is.na(dimension_units))
    for(m in dimensions.with.units) {
      unit_component <- paste0(component, "/Dimension_", m, "_units")
      rhdf5::h5write(dimension_units[[m]], fullname, unit_component)
    }
  }

  # Attach units
  if(!missing(units)) {
    rhdf5::h5write(units, fullname, paste0(component, "/units"))
  }

  rhdf5::h5closeAll()
}


