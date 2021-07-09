#' resolve_data_product
#'
#' @param handle \code{fdp} object
#' @param data_product a \code{string} specifying the name of the data product
#' @param file_type a \code{string} specifying the file type
#'
resolve_write <- function(handle, data_product, file_type) {

  check_yaml_write(handle, data_product)
  datastore <- handle$yaml$run_metadata$write_data_store

  # Check data product name isn't too long
  tmp <- get_fields("data_product")
  index <- which(tmp$field == "name")
  data_product_fields <- tmp[index, ]
  max_length_name <- data_product_fields$max_length
  if (nchar(data_product) > max_length_name)
    usethis::ui_stop(paste(usethis::ui_field(data_product),
                           "must be",
                           max_length_name, "characters or less"))

  # Get entry
  index <- lapply(handle$yaml$write, function(x)
    data_product == x$data_product) %>%
    unlist() %>% which()
  this_dp <- handle$yaml$write[[index]]

  # Get alias
  if ("use" %in% names(this_dp)) {
    alias <- this_dp$use
  } else {
    alias <- list()
  }

  # Get data product name
  if ("data_product" %in% names(alias)) {
    data_product <- alias$data_product
  } else {
    data_product <- this_dp$data_product
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
  public <- this_dp$public

  if (public == "true") {
    public <- TRUE
  } else if (public == "false") {
    public <- FALSE
  }

  # Extract / set save location
  if (data_product %in% handle$outputs$data_product) {
    tmp <- handle$outputs
    ind <- which(tmp$data_product == data_product)
    path <- unique(tmp$path[ind])

  } else {
    filename <- paste0(format(Sys.time(), "%Y%m%d-%H%M%S"), ".", file_type)
    path <- file.path(paste0(datastore, namespace), data_product, filename)
  }

  list(data_product = data_product,
       version = version,
       namespace = namespace,
       public = public,
       path = path)
}
