#' Create array-type H5 file
#'
#' Function to populate hdf5 file with array type data.
#'
#' @param array an \code{array} containing the data
#' @param handle list
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

  if (data_product %in% handle$outputs$data_product &
      component %in% handle$outputs$component) {
    usethis::ui_done("Component already recorded")
    invisible(handle$output_index(data_product, component, version))
  }

  # Extract metadata from config.yaml
  datastore <- handle$yaml$run_metadata$default_data_store
  namespace <- handle$yaml$run_metadata$default_output_namespace

  # Extract / set save location
  if (data_product %in% handle$outputs$data_product) {
    dp. <- data_product
    path <- handle$outputs %>%
      dplyr::filter(.data$data_product == dp.) %>%
      dplyr::select(.data$path) %>%
      unique() %>%
      unlist() %>%
      unname()

  } else {
    filename <- paste0(format(Sys.time(), "%Y%m%d-%H%M%S"), ".h5")
    path <- file.path(paste0(datastore, namespace), data_product, filename)
  }

  # Checks ------------------------------------------------------------------

  if(!grepl(".h5$", path)) stop("filename must be *.h5")
  if(!is.array(array)) stop("array must be an array")

  dimensions <- setdiff(dim(array), 1)
  for (i in length(dimensions))
    if(length(dimension_names[[i]]) != dim(array)[i])
      stop(paste0("Length of Dimension_", i,
                  "_names must equal the number of rows in array"))

  if(length(dimensions) != length(dimension_names))
    stop("Length of list dimension_names must equal the number of dimensions in array")

  # Save file ---------------------------------------------------------------

  # Generate directory structure
  directory <- dirname(path)
  if(!file.exists(directory)) dir.create(directory, recursive = TRUE)

  # Generate hdf5 file
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

  usethis::ui_done(paste("Writing component:",
                         usethis::ui_value(component), "\n",
                         "to data product:", usethis::ui_value(data_product)))

  index <- lapply(handle$yaml$write, function(x)
    data_product == x$data_product) %>%
    unlist() %>% which()
  this_dp <- handle$yaml$write[[index]]

  version <- this_dp$version

  handle$write_dataproduct(data_product,
                           path,
                           component,
                           description,
                           version)

  invisible(handle$output_index(data_product, component, version))
}
