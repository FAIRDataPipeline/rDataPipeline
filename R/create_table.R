#' Create table-type H5 file
#'
#' Function to populate hdf5 file with array type data.
#'
#' @param filename a \code{string} specifying the filename, e.g. "0.1.0.h5"
#' @param path a \code{string} specifying the directory in which you want to
#' save the h5 file; this will be automatically generated if it doesn't
#' already exist
#' @param component a \code{string} specifying a location within the hdf5 file,
#' e.g. "location/per_week/all_deaths"
#' @param df a \code{dataframe} containing the data
#' @param row_names (optional) a \code{vector} of rownames
#' @param column_units (optional) a \code{vector} comprising column units
#'
#' @family create functions
#'
#' @export
#'
#' @examples
#' df <- data.frame(column_1 = 1:2, column_2 = 3:4)
#' rownames(df) <- c("informative_rowname_1", "informative_rowname_2")
#' filename <- "test_table.h5"
#'
#' create_table(filename = filename,
#'              path = ".",
#'              component = "component_name",
#'              df = df,
#'              row_names = rownames(df),
#'              column_units = c(NA, "m^2"))
#'
#' file.remove(filename)
#'
create_table <- function(filename,
                         path = ".",
                         component,
                         df,
                         row_names,
                         column_units) {

  if(!grepl(".h5$", filename)) stop("filename must be *.h5")
  if(!is.data.frame(df)) stop("df must be a data.frame")

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
  rhdf5::h5write(df, fullname, paste0(component, "/table"))

  # Attach attributes
  if(!missing(row_names))
    rhdf5::h5write(row_names, fullname, paste0(component, "/row_names"))

  if(!missing(column_units))
    rhdf5::h5write(column_units, fullname, paste0(component, "/column_units"))

  rhdf5::h5closeAll()
}
