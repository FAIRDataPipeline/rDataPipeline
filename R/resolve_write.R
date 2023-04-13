#' resolve_data_product
#' 
#' @keywords internal
#'
#' @param handle an object of class \code{fdp, R6} containing metadata required
#' by the Data Pipeline API
#' @param data_product a \code{string} specifying the name of the data product
#' @param file_type (optional) a \code{string} specifying the file type; when
#' missing, \code{file_type} will be read from the config file
#'
resolve_write <- function(handle,
                          data_product,
                          file_type) {

  check_yaml_write(handle = handle,
                   data_product = data_product)

  datastore <- handle$yaml$run_metadata$write_data_store

  # Get entry
  write <- handle$yaml$write
  index <- get_index(write = write, data_product = data_product)
  this_write <- write[[index]]

  if (missing(file_type)) {
    file_type <- this_write$file_type
  }

  # Get alias
  if ("use" %in% names(this_write)) {
    alias <- this_write$use
  } else {
    alias <- list()
  }

  # Get data product name
  if ("data_product" %in% names(alias)) {
    write_dataproduct <- alias$data_product
  } else {
    write_dataproduct <- this_write$data_product

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
    version <- this_write$version
  }

  # Get public flag
  public <- this_write$use$public

  if (is.null(public)) {
    public <- TRUE
  } else if (tolower(public) == "true") {
    public <- TRUE
  } else if (tolower(public) == "false") {
    public <- FALSE
  }

  # Extract / set save location ---------------------------------------------

  # Check whether this data product has been written to in this Code Run
  # (could be a multi-component object)
  if (is.null(handle$outputs)) {
    file_exists <- FALSE
  } else {
    file_exists <- any(data_product == handle$outputs$data_product)
  }

  if (file_exists) {
    tmp <- handle$outputs
    ind <- which(tmp$use_data_product == data_product)
    path <- unique(tmp$path[ind])

  } else {
    filename <- paste0("dat-", random_hash(), ".", file_type)
    path <- file.path(paste0(datastore, namespace), write_dataproduct, filename)
  }

  list(data_product = write_dataproduct,
       description = this_write$description,
       version = version,
       namespace = namespace,
       public = public,
       path = path)
}
