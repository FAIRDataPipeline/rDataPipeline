#' resolve_data_product
#'
#' @param handle \code{fdp} object
#' @param data_product a \code{string} specifying the name of the data product
#' @param file_type a \code{string} specifying the file type
#'
resolve_write <- function(handle,
                          data_product,
                          file_type) {

  check_yaml_write(handle = handle,
                   data_product = data_product)

  datastore <- handle$yaml$run_metadata$write_data_store

  # Get entry
  write <- handle$yaml$write
  index <- get_index(write = write,
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
    # Different versions of the same data product may have the same
    # write_dataproduct, so filtering by that is not enough
    file_exists <- handle$outputs %>%
      filter(.data$data_product == write_dataproduct,
             .data$use_version == version,
             .data$use_namespace == namespace)
    file_exists <- nrow(file_exists) != 0
  }

  if (file_exists) {
    tmp <- handle$outputs
    ind <- which(tmp$data_product == data_product)
    path <- unique(tmp$path[ind])

  } else {
    filename <- paste0("dat-", random_hash(), ".", file_type)
    path <- file.path(paste0(datastore, namespace), write_dataproduct, filename)
  }

  list(data_product = write_dataproduct,
       description = this_dp$description,
       version = version,
       namespace = namespace,
       public = public,
       path = path)
}
