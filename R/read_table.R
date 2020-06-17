#' read_table
#'
#' @export
#'
read_table <- function(h5filename, path) {
  file.h5 <- H5File$new(h5filename, mode = "r")

  object <- file.h5[[paste0(path, "/table")]][]
  rownames(object) <- file.h5[[paste0(path, "/row_names")]][]

  file.h5$close_all()
  object
}
