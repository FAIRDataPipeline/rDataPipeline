#' read_table
#'
#' @param filepath a \code{string} specifying the path and filename of the file to
#' be read
#' @param component a \code{string} specifying a location within the hdf5 file
#'
#' @return Returns a \code{data.frame} with attached \code{column_units}
#' attributes,
#' if available
#'
#' @export
#'
#' @examples
#' df <- data.frame(a = 1:2, b = 3:4)
#' rownames(df) <- 1:2
#' filename <- "test_table.h5"
#'
#' create_table(filename = filename,
#'              path = ".",
#'              component = "level",
#'              df = df,
#'              row_names = rownames(df),
#'              column_units = c(NA, "m^2"))
#'
#' my_table <- read_table(filepath = filename,
#'                        component = "level")
#' attributes(my_table)
#'
#' file.remove(filename)
#'
read_table <- function(filepath,
                       component) {
  # Read hdf5 file
  file.h5 <- rhdf5::h5read(filepath, component)

  # Extract data object
  object <- file.h5$table

  # Attach rownames to object
  if(any("row_names" %in% names(file.h5)))
    rownames(object) <- file.h5$row_names

  # Attach remaining list elements as attributes
  ind <- grep("row_names|table", names(file.h5))
  tmp <- file.h5[-ind]

  for(i in seq_along(tmp)) {
    attr(object, names(tmp)[i]) <- tmp[[i]]
  }

  rhdf5::h5closeAll()
  object
}
