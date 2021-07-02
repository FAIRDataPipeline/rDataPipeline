#' resolve_read
#'
#' @param handle \code{fdp} object
#' @param data_product a \code{string} specifying the name of the data product
#' @param component a \code{string} specifying the name of data product
#' component
#'
resolve_read <- function(handle, data_product, component = NA) {

  read <- handle$yaml$read

  # If names(read) is not null, then a single entry has been added that
  # is not in a list, so put it in a list
  if (!all(is.null(names(read))))
    read <- list(read)

  # Find data product in `read:` section of config.yaml
  list_reads <- lapply(read, function(x) x$data_product) %>%
    unlist()

  index <- which(data_product %in% list_reads)

  if (length(index) == 0)
    usethis::ui_stop(paste(usethis::ui_field(data_product),
                           "not found in config file"))

  if(length(index) > 1)
    usethis::ui_stop("Multiple entries found in config file")

  this_read <- read[[index]]

  # Get aliases
  alias <- this_read$use

  # Get data product name
  if (any(names(alias) == "data_product")) {
    data_product <- this_read$use$data_product
  } else {
    data_product <- this_read$data_product
  }

  # Get component
  if (any(names(alias) == "component")) {
    component <- alias$component
  } else {
    component <- component
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

  namespace_id <- get_id("namespace", list(name = namespace))

  if (is.null(namespace_id))
    usethis::ui_stop(paste("default_input_namespace",
                           usethis::ui_field(namespace),
                           "not present in data registry"))

  assertthat::assert_that(length(namespace_id) == 1)

  # Get data product
  this_entry <- get_entry("data_product",
                          list(name = data_product,
                               version = version,
                               namespace = namespace_id))

  # Get data product path
  assertthat::assert_that(length(this_entry) == 1)
  this_object <- get_entity(this_entry[[1]]$object)
  this_location <- get_entity(this_object$storage_location)
  this_path <- this_location$path
  this_root <- get_entity(this_location$storage_root)$root

  # Get object component URL
  if (is.na(component)) {
    component_url <- get_url("object_component",
                             list(object = extract_id(this_object$url),
                                  whole_object = TRUE))
  } else {
    component_url <- get_url("object_component",
                             list(object = extract_id(this_object$url),
                                  name = component))
  }

  assertthat::assert_that(length(component_url) == 1)

  list(data_product = data_product,
       component = component,
       component_url = component_url,
       version = version,
       namespace = namespace,
       path = paste0(this_root, this_path))
}
