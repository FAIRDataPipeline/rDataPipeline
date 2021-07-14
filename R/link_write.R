#' link_write
#'
#' @param handle an object of class \code{fdp, R6} containing metadata required
#' by the Data Pipeline API
#' @param data_product a \code{string} representing an external object in the
#' config.yaml file
#' @param endpoint endpoint
#'
#' @export
#'
link_write <- function(handle, data_product, endpoint) {

  # Get metadata ------------------------------------------------------------

  write <- handle$yaml$write
  index <- write_index(index, write, data_product)

  this_write <- write[[index]]
  file_type <- this_write$file_type

  if (is.null(file_type)) {
    tmp <- "file_type"
    usethis::ui_stop(paste("Unknown", usethis::ui_field(tmp), "in",
                           usethis::ui_value(data_product),
                           "write block, please edit config file"))
  }

  write_metadata <- resolve_write(handle = handle,
                                  data_product = data_product,
                                  file_type = file_type,
                                  endpoint = endpoint)
  write_data_product <- write_metadata$data_product
  write_version <- write_metadata$version
  write_namespace <- write_metadata$namespace
  write_public <- write_metadata$public
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
                description = description,
                public = write_public)

  invisible(path)
}
