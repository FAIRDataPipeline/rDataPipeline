#' resolve_data_product
#'
#' @param handle \code{fdp} object
#' @param data_product a \code{string} specifying the name of the data product
#' @param file_type a \code{string} specifying the file type
#' @param endpoint endpoint
#'
resolve_write <- function(handle,
                          data_product,
                          file_type,
                          endpoint) {

  check_yaml_write(handle = handle,
                   data_product = data_product,
                   endpoint = endpoint)

  datastore <- handle$yaml$run_metadata$write_data_store

  # Get entry
  write <- handle$yaml$write
  index <- write_index(index = index,
                       write = write,
                       data_product = data_product)

  this_dp <- handle$yaml$write[[index]]

  # Get alias
  if ("use" %in% names(this_dp)) {
    alias <- this_dp$use
  } else {
    alias <- list()
  }

  # Get data product name
  if ("data_product" %in% names(alias)) {
    write_dataproduct <- alias$data_product
  } else {
    write_dataproduct <- this_dp$data_product

    if (basename(write_dataproduct) == "*") {
      write_dataproduct <- data_product
    }
  }

  # Get namespace
  if ("namespace" %in% names(alias)) {
    namespace <- alias$namespace
  } else {
    namespace <- handle$yaml$run_metadata$default_output_namespace
  }

  # Get version
  if ("version" %in% names(alias)) {
    version <- alias$version
  } else {
    version <- this_dp$version
  }

  # Get public flag
  public <- this_dp$use$public

  if (public == "true") {
    public <- TRUE
  } else if (public == "false") {
    public <- FALSE
  }

  # Extract / set save location
  if (write_dataproduct %in% handle$outputs$data_product) {
    tmp <- handle$outputs
    ind <- which(tmp$data_product == data_product)
    path <- unique(tmp$path[ind])

  } else {
    filename <- paste0(format(Sys.time(), "%Y%m%d-%H%M%S"), ".", file_type)
    path <- file.path(paste0(datastore, namespace), write_dataproduct, filename)
  }

  list(data_product = write_dataproduct,
       version = version,
       namespace = namespace,
       public = public,
       path = path)
}
