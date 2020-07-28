#' create_distribution
#'
#' Function to populate toml file with distribution type data.
#'
#' @param filename a \code{string} specifying the name of the toml file, e.g.
#' "0.1.0.toml"
#' @param path a \code{string} specifying the directory in which you want to save the toml file
#' @param name a \code{string} specifying the name of the parameter
#' @param distribution a \code{string} specifying the name of the distribution
#' @param parameters a named \code{list} specifying parameter values and
#' associated names (see example)
#'
#' @export
#'
#' @examples
#' filename <- "test.toml"
#' path <- "data-raw/example"
#' name <- "latency"
#' distribution = "gamma"
#' parameters <- list(shape = 2.0, scale = 3.0)
#' create_distribution(filename, path, name, distribution, parameters)
#'
create_distribution <- function(filename,
                                path,
                                name,
                                distribution,
                                parameters) {
  # Check file name
  if(!(grepl(".toml$", filename))) stop("filename must be *.toml")

  # Generate directory structure
  if(!file.exists(path)) dir.create(path, recursive = TRUE)
  if(missing(path)) path <- ""

  # Write toml
  tmp <- sapply(seq_along(parameters), function(x)
    paste0(names(parameters[x]), " = ", parameters[x], "\n")) %>%
    paste0(collapse = "")

  cat(paste0("[", name, "]\ntype = \"distribution\"\ndistribution = \"",
             distribution, "\"\n", tmp, "\n"), file = file.path(path, filename))
}
