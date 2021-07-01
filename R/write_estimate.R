#' Write estimate-type TOML file
#'
#' Function to populate toml file with point-estimate type data. If a file
#' already exists at the specified location, an additional component will be
#' added.
#'
#' @param value an object of class \code{numeric}
#' @param handle \code{fdp} object
#' @param data_product a \code{string} specifying the name of the data product
#' @param component a \code{string} specifying a location within the toml file
#' @param description a \code{string} describing the data product component
#'
#' @family write functions
#'
#' @export
#'
write_estimate <- function(value,
                           handle,
                           data_product,
                           component,
                           description) {

  # Get metadata ------------------------------------------------------------

  datastore <- handle$yaml$run_metadata$write_data_store

  write_metadata <- resolve_write(handle = handle,
                                  data_product = data_product,
                                  file_type = "toml")
  data_product <- write_metadata$data_product
  version <- write_metadata$version
  path <- write_metadata$path

  # Checks ------------------------------------------------------------------

  if (!is.numeric(value))
    usethis::ui_stop("{value} should be numeric")

  # If directory doesn't exist, generate directory structure
  directory <- dirname(path)
  if(!dir.exists(directory)) dir.create(directory, recursive = TRUE)

  # Write toml file ---------------------------------------------------------

  contents <- paste0("[", data_product, "]\n",
                     "type = \"point-estimate\"",
                     "\nvalue = ", value, "\n")

  if (file.exists(path)) {

    # Check that entry doesn't already exist
    existing <- read_estimate(path)
    if (data_product %in% names(existing))
      usethis::ui_stop("{data_product} is already listed in toml file")

    cat(contents, file = path, append = FALSE)

  } else {
    cat(paste0("\n", contents), file = path, append = TRUE)
  }

  cli::cli_alert_success(
    "Writing {.value {data_product}} to file")

  # Write to handle ---------------------------------------------------------

  handle$write_dataproduct(data_product,
                           path,
                           component,
                           description,
                           version)

  invisible(handle$output_index(data_product, component, version))
}
