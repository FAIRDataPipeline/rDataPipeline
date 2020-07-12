#' create_table
#'
#' Function to populate hdf5 file with array type data.
#'
#' @param filename a \code{string} specifying the filename, e.g. "0.1.0.h5"
#' @param component a \code{string} specifying a location within the hdf5 file,
#' e.g. "location/per_week/all_deaths"
#' @param df a \code{dataframe} containing the data
#' @param row_title (optional) a \code{string} descriptor of rownames
#' @param row_names (optional) a \code{vector} of rownames
#' @param column_units (optional) a \code{vector} comprising column units
#'
#' @export
#'
create_table <- function(filename,
                         component,
                         df,
                         row_title,
                         row_names,
                         column_units) {

  if(!grepl(".h5$", filename)) stop("filename must be *.h5")
  if(!is.data.frame(df)) stop("df must be a data.frame")

  # Generate hdf5 structure
  file.h5 <- H5File$new(filename)

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
  file.h5[[file.path(component, "table")]] <- df

  # Attach attributes
  if(!missing(row_title))
    file.h5[[file.path(component, "row_title")]] <- row_title
  if(!missing(row_names))
    file.h5[[file.path(component, "row_names")]] <- row_names
  if(!missing(column_units))
    file.h5[[file.path(component, "column_units")]] <- column_units

  file.h5$close_all()
}
