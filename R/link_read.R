#' link_read
#'
#' @param handle an object of class \code{fdp, R6} containing metadata required
#' by the Data Pipeline API
#' @param data_product a \code{string} representing an external object in the
#' config.yaml file
#'
#' @export
#'
link_read <- function(handle, data_product) {

  # If the data_product is already recorded in the handle, return the path
  if (!is.null(handle$inputs)) {
    if (data_product %in% handle$inputs$name) {
      output <- handle$inputs %>%
        dplyr::filter(.data$name == data_product) %>%
        dplyr::select(.data$path) %>%
        unlist() %>%
        unname()
      return(invisible(output))
    }
  }

  # If data_product is missing from config file, return an error
  list_reads <- lapply(handle$yaml$read, function(x) x$data_product) %>% unlist()
  missing_from_config <- !(data_product %in% list_reads)
  if (missing_from_config)
    usethis::ui_stop(paste(usethis::ui_field(data_product),
                           "missing from config file"))

  # Get data_product metadata
  tmp <- resolve_read(handle = handle,
                      data_product = data_product)
  read_dataproduct <- tmp$data_product
  read_component <- tmp$component
  read_component_url <- tmp$component_url
  read_version <- tmp$version
  read_namespace <- tmp$namespace
  read_path <- tmp$path

  usethis::ui_info(paste0("Locating ", usethis::ui_value(data_product)))

  # Store metadata in handle
  handle$input(data_product = data_product,
               use_data_product = read_dataproduct,
               use_component = read_component,
               use_version = read_version,
               use_namespace = read_namespace,
               path = read_path,
               component_url = read_component_url)

  # Return storage location
  read_path
}
