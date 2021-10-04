#' check_handle
#'
#' @param handle handle
#' @param data_product data_product
#' @param what c("inputs", "outputs")
#' @param component component
#'
check_handle <- function(handle, data_product, what, component) {

  # link_read() / link_write()
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
        this_dataproduct <- section[index,]

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
