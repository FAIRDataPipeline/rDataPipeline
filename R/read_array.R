#' read_array
#'
#' @export
#'
read_array <- function(h5filename, path) {
  file.h5 <- H5File$new(h5filename, mode = "r+")

  object <- file.h5[[paste0(path, "/array")]][,]
  if(!is.matrix(object)) object <- t(matrix(object))
  object <- as.data.frame(object)
  colnames(object) <- file.h5[[paste0(path, "/Dimension_2_names")]][]
  rownames(object) <- file.h5[[paste0(path, "/Dimension_1_names")]][]

  file.h5$close_all()
  object
}
