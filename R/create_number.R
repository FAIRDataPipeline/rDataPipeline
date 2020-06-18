#' create_number
#'
#' Function to populate toml file with number type data.
#'
#' @param toml_filename a \code{string} specifying the name of the toml file
#' @param value a \code{numeric} object specifying the value
#' @param name a \code{string} descriptor
#'
#' @export
#'
create_number <- function(toml_filename,
                          value,
                          name) {
  # Check file name
  if(!(grepl(".toml$", toml_filename))) stop("toml_filename must be toml.")

  # Write toml
  cat(paste0("[point-estimate]\nname = \"", name,
            "\"\nvalue = ", value, "\n"), file = toml_filename)
}
