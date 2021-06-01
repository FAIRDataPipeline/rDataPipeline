#' Create table-type H5 file
#'
#' Function to populate hdf5 file with array type data.
#'
#' @param df an \code{dataframe} containing the data
#' @param handle list
#' @param data_product a \code{string} specifying the name of the data product
#' @param component a \code{string} specifying a location within the hdf5 file,
#' @param row_names (optional) a \code{vector} of rownames
#' @param column_units (optional) a \code{vector} comprising column units
#'
#' @family write functions
#'
#' @export
#'
write_table <- function(df,
                        handle,
                        data_product,
                        component,
                        row_names,
                        column_units) {

  # Extract metadata from config.yaml
  datastore <- handle$yaml$run_metadata$default_data_store
  namespace <- handle$yaml$run_metadata$default_output_namespace

  # Extract / set save location
  if (data_product %in% names(handle$outputs)) {
    path <- unname(handle$outputs$dataproducts[[data_product]]$path)
  } else {
    filename <- paste0(openssl::sha1(as.character(Sys.time())), ".h5")
    path <- file.path(paste0(datastore, namespace), data_product, filename)
  }

  # Checks ------------------------------------------------------------------

  if(!grepl(".h5$", path)) stop("path must be *.h5")
  if(!is.data.frame(df)) stop("df must be a data.frame")

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
  rhdf5::h5write(df, path, paste0(component, "/table"))

  # Attach attributes
  if(!missing(row_names))
    rhdf5::h5write(row_names, path, paste0(component, "/row_names"))

  if(!missing(column_units))
    rhdf5::h5write(column_units, path, paste0(component, "/column_units"))

  rhdf5::h5closeAll()

  usethis::ui_done(paste("Added component:", usethis::ui_value(component), "\n",
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
