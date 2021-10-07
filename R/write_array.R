#' Write array component to HDF5 file
#'
#' Function to populate hdf5 file with array type data.
#'
#' @param array an \code{array} containing the data
#' @param handle an object of class \code{fdp, R6} containing metadata required
#' by the Data Pipeline API
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
#' @return Returns a handle index associated with the just written component,
#' which can be used to raise an issue if necessary
#'
#' @family write functions
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

  # If data product is already recorded in handle return index
  index <- check_handle(handle, data_product, "outputs", component)
  if (!is.null(index)) return(invisible(index))

  # If data product is missing from config file throw an error
  check_config(handle, data_product, "write")

  # Get metadata ------------------------------------------------------------

  write_metadata <- resolve_write(handle = handle,
                                  data_product = data_product,
                                  file_type = "h5")
  write_data_product <- write_metadata$data_product
  write_version <- write_metadata$version
  write_namespace <- write_metadata$namespace
  write_public <- write_metadata$public
  data_product_decription <- write_metadata$description
  path <- write_metadata$path

  # Check arguments ---------------------------------------------------------

  if (!is.array(array))
    stop("`array` must be an array")

  if (!missing(dimension_names)) {

    # Check dimensions class
    if (!all(unlist(lapply(dimension_names, is.vector))))
      usethis::ui_stop("Elements of dimension_names must be vectors")

    # Check number of dimensions
    if (length(dim(array)) != length(dimension_names)) {
      msg <- paste("Length of dimension_names does not equal number of",
                   "dimensions in array")
      usethis::ui_stop(msg)
    }

    # Check length of elements in each dimension
    if (any(unname(unlist(lapply(dimension_names, length))) != dim(array))) {
      msg <- paste("Number of elements in dimension_names does not equal",
                   "number of dimensions in array")
      usethis::ui_stop(msg)
    }
  }

  # Write hdf5 file ---------------------------------------------------------

  # Generate directory structure
  directory <- dirname(path)
  if (!dir.exists(directory)) dir.create(directory, recursive = TRUE)

  # Write hdf5 file
  if (file.exists(path)) {
    fid <- H5Fopen(path)
    if (length(h5ls(fid)) == 0) {
      current_structure <- ""
    } else {
      current_structure <- gsub("^/", "", unique(h5ls(fid)$group))
    }
    rhdf5::h5closeAll()

  } else {
    fid <- rhdf5::h5createFile(path)
    current_structure <- ""
  }

  # Generate internal structure
  if (grepl("/", component)) {
    directory.structure <- strsplit(component, "/")[[1]]
  } else {
    directory.structure <- component
  }

  for (i in seq_along(directory.structure)) {
    # This structure needs to be added
    if (i == 1) {
      build_structure <- directory.structure[1]
    } else {
      build_structure <- paste0(build_structure, "/", directory.structure[i])
    }
    # If the structure doesn't exist make it
    if (!build_structure %in% current_structure)
      rhdf5::h5createGroup(path, build_structure)
    # Update current structure
    current_structure <- c(current_structure, build_structure)
  }

  # Attach data
  rhdf5::h5write(array, path, paste0(component, "/array"))

  # Dimension names and titles ----------------------------------------------

  if (!missing(dimension_names)) {
    # Attach dimension titles
    dimension_titles <- names(dimension_names)

    for (i in seq_along(dimension_titles)) {
      attribute_component <- paste0(component, "/Dimension_", i, "_title")
      rhdf5::h5write(dimension_titles[i], path, attribute_component)
    }

    # Attach dimension names
    for (j in seq_along(dimension_names)) {
      attribute_component <- paste0(component, "/Dimension_", j, "_names")
      rhdf5::h5write(dimension_names[[j]], path, attribute_component)
    }
  }

  # Dimension values and units ----------------------------------------------

  # Attach dimension values
  if (!missing(dimension_values)) {
    dimensions.with.values <- which(!is.na(dimension_values))
    for(k in dimensions.with.values) {
      value_component <- paste0(component, "/Dimension_", k, "_values")
      rhdf5::h5write(dimension_values[[k]], path, value_component)
    }
  }

  # Attach dimension units
  if (!missing(dimension_units)) {
    dimensions_with_units <- which(!is.na(dimension_units))
    for (m in dimensions_with_units) {
      unit_component <- paste0(component, "/Dimension_", m, "_units")
      rhdf5::h5write(dimension_units[[m]], path, unit_component)
    }
  }

  # Attach units
  if (!missing(units)) {
    rhdf5::h5write(units, path, paste0(component, "/units"))
  }

  rhdf5::h5closeAll()

  cli::cli_alert_success(
    "Writing {.value {component}} to {.value {write_data_product}}")

  # Write to handle ---------------------------------------------------------

  handle$output(data_product = data_product,
                use_data_product = write_data_product,
                use_component = component,
                use_version = write_version,
                use_namespace = write_namespace,
                path = path,
                data_product_description = data_product_decription,
                component_description = description,
                public = write_public)

  # Return handle index -----------------------------------------------------

  index <- handle$output_index(data_product = write_data_product,
                               component = component,
                               version = write_version,
                               namespace = write_namespace)
  invisible(index)
}
