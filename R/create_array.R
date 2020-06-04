#' create_array
#'
#' Function to populate hdf5 file with array type data.
#'
#' @param filename a \code{string} specifying the name of the hdf5 file
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
#' @examples
#' \dontrun{
#' filename <- "array.h5"
#' component <- "dz/total"
#' array <- matrix(1:10, 5)
#' colnames(array) <- paste0("age", 1:2)
#' rownames(array) <- paste0("dz", 1:5)
#' dimension_names <- list(`age classes` = colnames(array),
#' `area names` = rownames(array))
#'
#' dimension_values <- list(NA, data.frame(x = 1:2, y = 3:4))
#' dimension_units <- list(NA, "apples")
#' units <- "seconds"
#'
#' create_array(filename, component, array, dimension_names,
#' dimension_values, dimension_units, units)
#'
#' file.h5 <- H5File$new(filename)
#' file.h5$ls(recursive = TRUE)
#' file.h5[["dz/total/Dimension_1_names"]][]
#' file.h5[["dz/total/Dimension_1_title"]][]
#' file.h5[["dz/total/Dimension_2_names"]][]
#' file.h5[["dz/total/Dimension_2_title"]][]
#' file.h5[["dz/total/Dimension_2_units"]][]
#' file.h5[["dz/total/Dimension_2_values"]][]
#' file.h5[["dz/total/units"]][]
#' file.h5$close_all()
#' }
#'
create_array <- function(filename,
                         component,
                         array,
                         dimension_names,
                         dimension_values,
                         dimension_units,
                         units) {

  file.h5 <- H5File$new(filename)
  current.groups <- names(file.h5)

  if(grepl("/", component)) { # If there's a subgroup as well as a group

    tmp <- strsplit(component, "/")[[1]]
    this.group <- tmp[1]

    if(this.group %in% current.groups)
      group <- file.h5[[this.group]] else
        group <- file.h5$create_group(this.group)

    this.subgroup <- tmp[2]
    current.subgroups <- names(group)

    if(this.subgroup %in% current.subgroups)
      location <- group[[this.subgroup]] else
        location <- group$create_group(this.subgroup)


  } else { # If there's only a group
    this.group <- component

    if(this.group %in% current.groups)
      location <- file.h5[[this.group]] else
        location <- file.h5$create_group(this.group)
  }

  # Attach data
  location[["array"]] <- array

  dimension_titles <- names(dimension_names)

  # Attach row attributes
  location[["Dimension_1_title"]] <- dimension_titles[1]
  location[["Dimension_1_names"]] <- dimension_names[[1]]

  # Attach column atttributes
  location[["Dimension_2_title"]] <- dimension_titles[2]
  location[["Dimension_2_names"]] <- dimension_names[[2]]

  # Attach additional attributes
  if(!missing(dimension_values)) {
    dimensions.with.values <- which(!is.na(dimension_values))

    for(i in dimensions.with.values)
      eval(parse(text = paste0("location[[\"Dimension_", i,
                               "_values\"]] <- dimension_values[[i]]")))
  }

  if(!missing(dimension_units)) {
    dimensions.with.units <- which(!is.na(dimension_units))

    for(i in dimensions.with.units)
      eval(parse(text = paste0("location[[\"Dimension_", i,
                               "_units\"]] <- dimension_units[[i]]")))
  }

  if(!missing(units))
      eval(parse(text = paste0("location[[\"units\"]] <- units")))

  file.h5$close_all()
}


