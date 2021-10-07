#' fdp_resolve_write
#'
#' @param this_write this_write
#' @param yaml user written config file
#'
fdp_resolve_write <- function(this_write, yaml) {

  endpoint <- yaml$run_metadata$local_data_registry_url

  # Get alias
  if ("use" %in% names(this_write)) {
    alias <- this_write$use
  } else {
    alias <- list()
  }

  # Get data product
  if ("data_product" %in% names(alias)) {
    write_dataproduct <- alias$data_product
  } else {
    write_dataproduct <- this_write$data_product
  }

  # Get namespace
  if ("namespace" %in% names(alias)) {
    write_namespace <- alias$namespace
  } else {
    write_namespace <- yaml$run_metadata$default_output_namespace
  }

  write_namespace_url <- new_namespace(name = write_namespace,
                                       endpoint = endpoint)
  write_namespace_id <- extract_id(write_namespace_url, endpoint = endpoint)

  # Get public flag
  if ("public" %in% names(this_write)) {
    write_public <- this_write$public
  } else {
    write_public <- "true"
  }

  if (tolower(write_public) == "true") {
    write_public <- TRUE
  } else if (tolower(write_public) == "false") {
    write_public <- FALSE
  } else {
    stop("public value not recognised")
  }

  if ("version" %in% names(this_write)) {
    # Get version from before `use:` block
    write_version <- resolve_version(version = this_write$version,
                                     data_product = write_dataproduct,
                                     namespace_id = write_namespace_id)

  } else if ("version" %in% names(alias)) {
    # Get version from inside `use:` block
    write_version <- resolve_version(version = alias$version,
                                     data_product = write_dataproduct,
                                     namespace_id = write_namespace_id)

  } else {
    # If version is missing, increment by PATCH
    entries <- get_entry("data_product",
                         list(name = write_dataproduct,
                              namespace = write_namespace_id),
                         endpoint = endpoint)

    if (is.null(entries)) {
      write_version <- "0.0.1"

    } else {
      tmp <- lapply(entries, function(x) x$version) %>%
        unlist() %>%
        semver::parse_version() %>%
        max()
      patch <- tmp$patch
      tmp$patch <- as.integer(patch + 1)
      write_version <- as.character(tmp)
    }
  }

  list(write_dataproduct = write_dataproduct,
       write_public = write_public,
       write_version = write_version,
       write_namespace_id = write_namespace_id)
}
