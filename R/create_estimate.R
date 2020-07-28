#' create_estimate
#'
#' Function to populate toml file with number type data.
#'
#' @param filename a \code{string} specifying the name of the toml file, e.g.
#' "0.1.0.toml"
#' @param path a \code{string} specifying the directory in which you want to save the toml file
#' @param parameters a \code{list} (see example, below)
#'
#' @export
#'
#' @examples
#' filename <- "test.toml"
#' path <- "data-raw/example"
#' parameters <- list(asymptomatic_period = 192.0)
#' create_estimate(filename, path, parameters)
#'
#' filename <- "anothertest.toml"
#' path <- "data-raw/example"
#' parameters <- list(asymptomatic_period = 192.0, latent_period = 123.12)
#' create_estimate(filename, path, parameters)
#'
create_estimate <- function(filename,
                            path,
                            parameters) {
  # Check file name
  if(!(grepl(".toml$", filename))) stop("filename must be *.toml")

  # Generate directory structure
  if(!file.exists(path)) dir.create(path, recursive = TRUE)
  if(missing(path)) path <- ""

  # Write toml
  tmp <- lapply(seq_along(parameters), function(i) {
    paste0("[", names(parameters)[[i]], "]\ntype = \"point-estimate\"",
           "\nvalue = ", parameters[[i]], "\n")
  })

  cat(paste(tmp, collapse = "\n"), file = file.path(path, filename))
}
