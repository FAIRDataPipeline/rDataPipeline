#' resolve_read
#'
#' @param handle \code{fdp} object
#' @param data_product a \code{string} specifying the name of the data product
#'
resolve_read <- function(handle, data_product) {

  read <- handle$yaml$read

  # If names(read) is not null, then a single entry has been added that
  # is not in a list, so put it in a list
  if (!all(is.null(names(read))))
    read <- list(read)

  # Find data product in `read:` section of config.yaml
  index <- lapply(seq_along(read), function(x)
    read[[x]]$data_product == data_product &&
      read[[x]]$component == component) %>%
    unlist() %>%
    which()

  this_read <- read[[index]]

  # Get aliases
  alias <- this_read$use

  # Get data product name
  if (any(names(alias) == "data_product")) {
    data_product <- this_read$use$data_product
  } else {
    data_product <- this_read$data_product
  }

  # Get component name
  if (any(names(alias) == "component")) {
    component <- this_read$use$component
  } else {
    component <- this_read$component
  }

  # Get version number
  if (any(names(alias) == "version")) {
    version <- this_read$use$version
  } else {
    version <- this_read$version
  }

  # Get namespace
  if (any(names(alias) == "namespace")) {
    namespace <- alias$namespace
  } else {
    namespace <- handle$yaml$run_metadata$default_input_namespace
  }

  namespace_url <- get_url("namespace", list(name = namespace))
  namespace_id <- extract_id(namespace_url)

  this_entry <- get_entry("data_product",
                          list(name = data_product,
                               version = version,
                               namespace = namespace_id))
  assertthat::assert_that(length(this_entry) == 1)
  this_entry <- this_entry[[1]]

  this_object <- get_entity(this_entry$object)
  this_location <- get_entity(this_object$storage_location)
  this_location$path
}
