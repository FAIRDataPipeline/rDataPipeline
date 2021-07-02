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

  # Get metadata ------------------------------------------------------------

  write <- handle$yaml$write
  index <- lapply(write, function(x)
    which(data_product == x$data_product)) %>% unlist()

  if (length(index) == 0)
    usethis::ui_stop("{data_product} not present in config.yaml")

  this_write <- write[[index]]
  file_type <- this_write$file_type

  write_metadata <- resolve_write(handle = handle,
                                  data_product = data_product,
                                  file_type = file_type)
  write_data_product <- write_metadata$data_product
  write_version <- write_metadata$version
  write_namespace <- write_metadata$namespace
  path <- write_metadata$path

  description <- this_write$description

  # Generate directory structure --------------------------------------------

  directory <- dirname(path)
  if(!file.exists(directory)) dir.create(directory, recursive = TRUE)

  # Write to handle ---------------------------------------------------------

  handle$output(data_product = data_product,
                use_data_product = write_data_product,
                use_component = NA,
                use_version = write_version,
                use_namespace = write_namespace,
                path = path,
                description = description)

  invisible(path)
}
