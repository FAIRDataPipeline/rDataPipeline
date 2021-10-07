#' Write table component to HDF5 file
#'
#' Function to populate hdf5 file with array type data.
#'
#' @param df an \code{dataframe} containing the data
#' @param handle an object of class \code{fdp, R6} containing metadata required
#' by the Data Pipeline API
#' @param data_product a \code{string} specifying the name of the data product
#' @param component a \code{string} specifying a location within the hdf5 file,
#' @param description a \code{string} describing the data product component
#' @param row_names (optional) a \code{vector} of rownames
#' @param column_units (optional) a \code{vector} comprising column units
#'
#' @return Returns a handle index associated with the just written component,
#' which can be used to raise an issue if necessary
#'
#' @family write functions
#' @export
#'
write_table <- function(df,
                        handle,
                        data_product,
                        component,
                        description,
                        row_names,
                        column_units) {

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

  # Checks ------------------------------------------------------------------

  if (!is.data.frame(df)) stop("df must be a data.frame")

  # Write hdf5 file ---------------------------------------------------------

  # Generate directory structure
  directory <- dirname(path)
  if (!file.exists(directory)) dir.create(directory, recursive = TRUE)

  # Generate hdf5 file
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
    directory_structure <- strsplit(component, "/")[[1]]
  } else {
    directory_structure <- component
  }

  for (i in seq_along(directory_structure)) {
    # This structure needs to be added
    if (i == 1) {
      build.structure <- directory_structure[1]
    } else {
      build.structure <- paste0(build.structure, "/", directory_structure[i])
    }
    # If the structure doesn't exist make it
    if (!build.structure %in% current_structure)
      rhdf5::h5createGroup(path, build.structure)
    # Update current structure
    current_structure <- c(current_structure, build.structure)
  }

  # Attach data
  rhdf5::h5write(df, path, paste0(component, "/table"))

  # Attach attributes
  if (!missing(row_names))
    rhdf5::h5write(row_names, path, paste0(component, "/row_names"))

  if (!missing(column_units))
    rhdf5::h5write(column_units, path, paste0(component, "/column_units"))

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

  index <- handle$output_index(data_product = write_data_product,
                               component = component,
                               version = write_version,
                               namespace = write_namespace)
  invisible(index)
}
