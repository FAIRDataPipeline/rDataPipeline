#' add_read
#'
#' Add data product to `read` block of user-written config file. Used in
#' combination with \code{create_config()} for unit testing.
#'
#' @param path config file path
#' @param data_product data_product field
#' @param component component field
#' @param version (optional) version field
#' @param use_data_product (optional) use_data_product field
#' @param use_component (optional) use_component field
#' @param use_version (optional) use_version field
#' @param use_namespace (optional) use_namespace field
#'
#' @export
#'
#' @examples
#' \dontrun{
#' path <- "test_config/config.yaml"
#'
#' # Write run_metadata block
#' create_config(path = path,
#'               description = "test",
#'               input_namespace = "test_user",
#'               output_namespace = "test_user")
#'
#' # Write read block
#' add_read(path = path,
#'          data_product = "test/array",
#'          component = "level/a/s/d/f/s",
#'          version = "0.2.0")
#' }
#'
add_read <- function(path,
                     data_product,
                     component,
                     version,
                     use_data_product,
                     use_component,
                     use_version,
                     use_namespace) {

  # Generate read block
  new_read <- list()
  new_read$data_product <- data_product

  if (!missing(component)) new_read$component <- component
  if (!missing(version)) new_read$version <- version
  if (!missing(use_data_product)) new_read$use$data_product <- use_data_product
  if (!missing(use_component)) new_read$use$component <- use_component
  if (!missing(use_version)) new_read$use$version <- use_version
  if (!missing(use_namespace)) new_read$use$namespace <- use_namespace

  # Read file contents
  contents <- configr::read.config(file = path)

  run_metadata <- contents$run_metadata

  if ("read" %in% names(contents)) {
    read <- contents$read
    index <- length(read) + 1
    read[[index]] <- new_read
  } else {
    read <- list(new_read)
  }

  # Write working config.yaml file
  if ("write" %in% names(contents)) {
    yaml::write_yaml(list(run_metadata = run_metadata,
                          write = contents$write,
                          read = read),
                     file = path)
  } else {
    yaml::write_yaml(list(run_metadata = run_metadata,
                          read = read),
                     file = path)
  }

}
