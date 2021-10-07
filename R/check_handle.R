#' check_handle
#'
#' @param handle an object of class \code{fdp, R6} containing metadata required
#' by the Data Pipeline API
#' @param data_product a \code{string} specifying the name of the data product
#' @param what element in handle -- one of c("inputs", "outputs")
#' @param component a \code{string} specifying the name of the component
#'
check_handle <- function(handle, data_product, what, component) {

  # link_read() / link_write() functions
  if (missing(component)) {
    if (!is.null(handle[[what]])) {
      section <- handle[[what]]

      if (data_product %in% section$data_product) {
        index <- which(data_product %in% section$data_product)
        path <- section$path[index]
        return(path)
      } else {
        return(NULL)
      }
    }

    # internal formats
  } else {

    if (!is.null(handle[[what]])) {
      section <- handle[[what]]

      if (data_product %in% section$data_product) {
        index <- which(data_product %in% section$data_product)
        this_dataproduct <- section[index, ]

        if (component %in% this_dataproduct$use_component) {
          index <- which(component %in% this_dataproduct$use_component)
          handle_index <- this_dataproduct$index[index]
          return(handle_index)
        } else {
          return(NULL)
        }
      }
    }
  }
}
