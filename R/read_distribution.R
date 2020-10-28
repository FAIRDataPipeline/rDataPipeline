#' Read distribution from TOML file
#'
#' Function to read disdtribution type data from toml file.
#'
#' @param filepath a \code{string} specifying the path and filename of the file
#' to be read
#'
#' @export
#'
#' @examples
#' # Write a single distribution into a toml file
#' dist <- list(name = "latency",
#'              distribution = "gamma",
#'              parameters = list(shape = 2.0, scale = 3.0))
#' filename <- "test_single.toml"
#'
#' create_distribution(filename = filename,
#'                     path = ".",
#'                     distribution = dist)
#'
#' read_distribution(filepath = filename)
#'
#' file.remove(filename)
#'
#' # Write multiple distributions into a toml file
#' dist1 <- list(name = "latency",
#'               distribution = "gamma",
#'               parameters = list(shape = 2.0, scale = 3.0))
#' dist2 <- list(name = "virulence",
#'               distribution = "gamma",
#'               parameters = list(shape = 2.0, scale = 3.0))
#' filename <- "test_multi.toml"
#'
#' create_distribution(filename = filename,
#'                     path = ".",
#'                     distribution = list(dist1, dist2))
#'
#' read_distribution(filepath = filename)
#'
#' file.remove(filename)
#'
read_distribution <- function(filepath) {
  # Read file
  tmp <- configr::read.config(file = filepath)

  # Check file
  if(tmp[[1]]$type != "distribution")
    stop("The file you are trying to read does not contain a distribution")

  tmp
}
