#' create_number
#'
#' Function to populate toml file with number type data.
#'
#' @param filename a \code{string} specifying the name of the toml file
#' @param value a \code{numeric} object specifying the value
#' @param name a \code{string} descriptor
#'
#' @export
#'
#' @examples
#' filename <- "test.toml"
#' value <- 2.0
#' name <- "latency"
#' create_number(filename, value, name)
#'
create_number <- function(filename,
                          value,
                          name) {
  # Check file name
  if(!(grepl(".toml$", filename))) stop("toml_filename must be *.toml")

  # Write toml
  cat(paste0("[", name, "]\ntype = \"point-estimate\"",
            "\nvalue = ", value, "\n"), file = filename)
}
