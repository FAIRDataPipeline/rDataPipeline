#' add_parameter_from_paper
#'
#'
#'
add_parameter_from_paper <- function(value,
                                     parameter_name,
                                     paper,
                                     authors,
                                     release_date,
                                     journal,
                                     abstract,
                                     storage_location) {

  # Add source data ---------------------------------------------------------

  new_object()
  new_storage_location()
  # new_accessibility() assume public
  new_external_object()
  new_source()

  # Add processing script ---------------------------------------------------

  new_object()
  new_storage_location()
  # new_accessiblity() assume public
  new_code_repo_version()

  # Add data product --------------------------------------------------------

  new_object()
  new_object_component()
  new_storage_location()
  new_data_product()

}
