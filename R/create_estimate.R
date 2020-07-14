#' create_estimate
#'
#' Function to populate toml file with number type data.
#'
#' @param filename a \code{string} specifying the name of the toml file, e.g.
#' "0.1.0.toml"
#' @param path a \code{string} specifying the name of the toml file (and path)
#' @param value a \code{numeric} object specifying the value
#' @param name a \code{string} descriptor
#'
#' @export
#'
#' @examples
#' filename <- "test.toml"
#' path <- "data-raw/latency"
#' value <- 2.0
#' name <- "latency"
#' create_estimate(filename, path, value, name)
#'
create_estimate <- function(filename,
                            path,
                            value,
                            name) {
  # Check file name
  if(!(grepl(".toml$", filename))) stop("filename must be *.toml")

  # Generate directory structure
  if(!file.exists(path)) dir.create(path, recursive = TRUE)
  if(missing(path)) path <- ""

  # Write toml
  cat(paste0("[", name, "]\ntype = \"point-estimate\"",
             "\nvalue = ", value, "\n"), file = file.path(path, filename))
}
