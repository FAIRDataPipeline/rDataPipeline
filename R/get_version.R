#' get_version
#'
#' @param handle handle
#' @param data_product data_product
#' @param namespace namespace
#'
#' @return Returns a character string specifying the version number
#'
get_version <- function(handle, data_product, namespace) {

  index <- lapply(handle$yaml$write, function(x)
    data_product == x$data_product) %>%
    unlist() %>% which()
  this_dp <- handle$yaml$write[[index]]

  if ("version" %in% names(this_dp)) {
    return(this_dp$version)

  } else {
    #  run_server()
    # a <- get_entry("data_product", list(name = data_product, namespace = namespace))
    #  stop_server()
    stop("need to write this")
  }
}
