#' fdp_resolve_read
#'
#' @param this_read this_read
#' @param yaml user written config file
#'
fdp_resolve_read <- function(this_read, yaml) {

  endpoint <- yaml$run_metadata$local_data_registry_url

  # Get alias
  if ("use" %in% names(this_read)) {
    alias <- this_read$use
  } else {
    alias <- list()
  }

  # Get version
  if ("version" %in% names(this_read)) {
    read_version <- this_read$version

  } else if ("version" %in% names(alias)) {
    read_version <- alias$version

  } else {

    # Get data product
    if ("data_product" %in% names(alias)) {
      read_dataproduct <- alias$data_product
    } else {
      read_dataproduct <- this_read$data_product
    }

    # Get namespace
    if ("namespace" %in% names(alias)) {
      read_namespace <- alias$namespace
    } else {
      read_namespace <- yaml$run_metadata$default_input_namespace
    }

    read_namespace_url <- new_namespace(name = read_namespace,
                                        endpoint = endpoint)
    read_namespace_id <- extract_id(read_namespace_url, endpoint = endpoint)

    entries <- get_entry("data_product",
                         list(name = read_dataproduct,
                              namespace = read_namespace_id))
    if (is.null(entries)) {
      usethis::ui_stop("{read_dataproduct} is not in local registry")

    } else {
      read_version <- lapply(entries, function(x) x$version) %>%
        unlist() %>%
        max()
    }
  }

  read_version
}
