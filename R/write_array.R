#' Create array-type H5 file
#'
#' Function to populate hdf5 file with array type data.
#'
#' @param array an \code{array} containing the data
#' @param handle \code{fdp} object
#' @param data_product a \code{string} specifying the name of the data product
#' @param component a \code{string} specifying a location within the hdf5 file
#' @param description a \code{string} describing the data product component
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
#' @family write functions
#'
#' @export
#'
write_array <- function(array,
                        handle,
                        data_product,
                        component,
                        description,
                        dimension_names,
                        dimension_values,
                        dimension_units,
                        units) {

  # Get metadata ------------------------------------------------------------

  datastore <- handle$yaml$run_metadata$write_data_store

  write_metadata <- resolve_write(handle = handle,
                                  data_product = data_product,
                                  file_type = "h5")
  data_product <- write_metadata$data_product
  version <- write_metadata$version
  path <- write_metadata$path

  # Checks ------------------------------------------------------------------

  if (!is.array(array))
    stop("`array` must be an array")

  if (!missing(dimension_names)) {

    # Check dimensions class
    if (!all(unlist(lapply(dimension_names, is.vector))))
      usethis::ui_stop("Elements of dimension_names must be vectors")

    # Check number of dimensions
    if (length(dim(array)) != length(dimension_names))
      usethis::ui_stop("Length of dimension_names does not equal number of dimensions in array")

    # Check length of elements in each dimension
    if (any(unname(unlist(lapply(dimension_names, length))) != dim(array)))
      usethis::ui_stop("Number of elements in dimension_names does not equal number of dimensions in array")
  }

  # Write hdf5 file ---------------------------------------------------------

  # Generate directory structure
  directory <- dirname(path)
  if(!dir.exists(directory)) dir.create(directory, recursive = TRUE)

  # Write hdf5 file
  if(file.exists(path)) {
    fid <- H5Fopen(path)
    if(length(h5ls(fid)) == 0) {
      current.structure <- ""
    } else {
      current.structure <- gsub("^/", "", unique(h5ls(fid)$group))
    }
    rhdf5::h5closeAll()

  } else {
    fid <- rhdf5::h5createFile(path)
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
      rhdf5::h5createGroup(path, build.structure)
    # Update current structure
    current.structure <- c(current.structure, build.structure)
  }

  # Attach data
  rhdf5::h5write(array, path, paste0(component, "/array"))

  # Dimension names and titles ----------------------------------------------

  if (!missing(dimension_names)) {
    # Attach dimension titles
    dimension_titles <- names(dimension_names)

    for(i in seq_along(dimension_titles)) {
      attribute_component <- paste0(component, "/Dimension_", i, "_title")
      rhdf5::h5write(dimension_titles[i], path, attribute_component)
    }

    # Attach dimension names
    for(j in seq_along(dimension_names)) {
      attribute_component <- paste0(component, "/Dimension_", j, "_names")
      rhdf5::h5write(dimension_names[[j]], path, attribute_component)
    }
  }

  # Dimension values and units ----------------------------------------------

  # Attach dimension values
  if(!missing(dimension_values)) {
    dimensions.with.values <- which(!is.na(dimension_values))
    for(k in dimensions.with.values) {
      value_component <- paste0(component, "/Dimension_", k, "_values")
      rhdf5::h5write(dimension_values[[k]], path, value_component)
    }
  }

  # Attach dimension units
  if(!missing(dimension_units)) {
    dimensions.with.units <- which(!is.na(dimension_units))
    for(m in dimensions.with.units) {
      unit_component <- paste0(component, "/Dimension_", m, "_units")
      rhdf5::h5write(dimension_units[[m]], path, unit_component)
    }
  }

  # Attach units
  if(!missing(units)) {
    rhdf5::h5write(units, path, paste0(component, "/units"))
  }

  rhdf5::h5closeAll()

  cli::cli_alert_success(
    "Writing {.value {component}} to {.value {data_product}}")

  # Write to handle ---------------------------------------------------------

  handle$write_dataproduct(data_product,
                           path,
                           component,
                           description,
                           version)

  invisible(handle$output_index(data_product, component, version))
}
