#' write_dataproduct
#'
#' @param path config file path
#' @param data_product data_product field
#' @param description component field
#' @param version (optional) version version field
#' @param use_data_product (optional) use_data_product use_data_product field
#' @param use_component (optional) use_component use_component field
#' @param use_version (optional) use_version use_version field
#' @param use_namespace (optional) use_namespace use_namespace field
#'
#' @export
#'
write_dataproduct <- function(path,
                              data_product,
                              description,
                              version,
                              file_type,
                              use_data_product,
                              use_component,
                              use_version,
                              use_namespace) {

  # Generate write block
  new_write <- list()
  new_write$data_product <- data_product
  new_write$description <- description

  if (!missing(version)) new_write$version <- version
  if (!missing(file_type)) new_write$file_type <- file_type
  if (!missing(use_data_product)) new_write$use$data_product <- use_data_product
  if (!missing(use_component)) new_write$use$component <- use_component
  if (!missing(use_version)) new_write$use$version <- use_version
  if (!missing(use_namespace)) new_write$use$namespace <- use_namespace

  # Read file contents
  contents <- configr::read.config(file = path)

  run_metadata <- contents$run_metadata

  if ("write" %in% names(contents)) {
    write <- contents$write
    index <- length(write) + 1
    write[[index]] <- new_write
  } else {
    write <- list(new_write)
  }

  # Write working config.yaml file
  if ("read" %in% names(contents)) {
    yaml::write_yaml(list(run_metadata = run_metadata,
                          write = write,
                          read = contents$read),
                     file = path)
  } else {
    yaml::write_yaml(list(run_metadata = run_metadata,
                          write = write),
                     file = path)
  }

}
