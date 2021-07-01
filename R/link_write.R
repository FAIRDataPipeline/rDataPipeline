#' link_write
#'
#' @param handle an object of class \code{fdp, R6} containing metadata required
#' by the Data Pipeline API
#' @param data_product a \code{string} representing an external object in the
#' config.yaml file
#'
#' @export
#'
link_write <- function(handle, data_product) {

  # Get object metadata from working config.yaml
  write <- handle$yaml$write
  index <- lapply(write, function(x)
    data_product == x$data_product) %>% unlist() %>% which()

  if (length(index) == 0)
    usethis::ui_stop("{data_product} not present in config.yaml")

  this_write <- write[[index]]

  if ("use" %in% names(this_write)) {
    alias <- this_write$use
  } else {
    alias <- list()
  }

  datastore <- handle$yaml$run_metadata$write_data_store

  if ("namespace" %in% names(alias)) {
    write_namespace <- alias$namespace
  } else {
    write_namespace <- handle$yaml$run_metadata$default_output_namespace
  }

  if ("data_product" %in% names(alias)) {
    write_data_product <- alias$data_product
  } else {
    write_data_product <- this_write$data_product
  }

  description <- this_write$description
  write_version <- this_write$use$version

  file_type <- this_write$file_type
  filename <- paste0(format(Sys.time(), "%Y%m%d-%H%M%S"), ".", file_type)

  path <- file.path(paste0(datastore, write_namespace),
                    write_data_product,
                    filename)

  # Generate directory structure
  directory <- dirname(path)
  if(!file.exists(directory)) dir.create(directory, recursive = TRUE)

  handle$write_dataproduct(write_data_product,
                           path,
                           component = NA,
                           description,
                           write_version)

  invisible(path)
}
