#' create_table
#' @export
#'
create_table <- function(filename,
                         component,
                         df,
                         row_title,
                         row_names,
                         column_units) {

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
  location[["table"]] <- df

  # Attach attributes
  if(!missing(row_title)) location[["row_title"]] <- row_title
  if(!missing(row_names)) location[["row_names"]] <- row_names
  if(!missing(column_units)) location[["column_units"]] <- column_units

  file.h5$close_all()
}
