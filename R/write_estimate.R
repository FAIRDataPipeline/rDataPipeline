#' Write estimate component to TOML file
#'
#' Function to populate toml file with point-estimate type data. If a file
#' already exists at the specified location, an additional component will be
#' added.
#'
#' @param value an object of class \code{numeric}
#' @param handle an object of class \code{fdp, R6} containing metadata required
#' by the Data Pipeline API
#' @param data_product a \code{string} specifying the name of the data product
#' @param component a \code{string} specifying a location within the toml file
#' @param description a \code{string} describing the data product component
#'
#' @return Returns a handle index associated with the just written component,
#' which can be used to raise an issue if necessary
#'
#' @family write functions
#' @export
#'
write_estimate <- function(value,
                           handle,
                           data_product,
                           component,
                           description) {

  # If data product is already recorded in handle return index
  index <- check_handle(handle, data_product, "outputs", component)
  if (!is.null(index)) return(invisible(index))

  # If data product is missing from config file throw an error
  check_config(handle, data_product, "write")

  # Get metadata ------------------------------------------------------------

  write_metadata <- resolve_write(handle = handle,
                                  data_product = data_product,
                                  file_type = "toml")
  write_data_product <- write_metadata$data_product
  write_version <- write_metadata$version
  write_namespace <- write_metadata$namespace
  write_public <- write_metadata$public
  data_product_decription <- write_metadata$description
  path <- write_metadata$path

  # Checks ------------------------------------------------------------------

  if (!is.numeric(value))
    usethis::ui_stop("{value} should be numeric")

  # If directory doesn't exist, generate directory structure
  directory <- dirname(path)
  if (!dir.exists(directory)) dir.create(directory, recursive = TRUE)

  # Write toml file ---------------------------------------------------------

  contents <- paste0("[", component, "]\n",
                     "type = \"point-estimate\"",
                     "\nvalue = ", value, "\n")

  if (file.exists(path)) {

    # Check that component doesn't already exist
    existing <- configr::read.config(file = path)
    if (component %in% names(existing))
      usethis::ui_stop("{component} is already listed in toml file")

    cat(paste0("\n", contents), file = path, append = TRUE)

  } else {
    cat(contents, file = path, append = FALSE)
  }

  cli::cli_alert_success(
    "Writing {.value {write_data_product}} to file")

  # Write to handle ---------------------------------------------------------

  handle$output(data_product = data_product,
                use_data_product = write_data_product,
                use_component = component,
                use_version = write_version,
                use_namespace = write_namespace,
                path = path,
                data_product_description = data_product_decription,
                component_description = description,
                public = write_public)

  invisible(handle$output_index(data_product = data_product,
                                component = component,
                                version = write_version,
                                namespace = write_namespace))
}
