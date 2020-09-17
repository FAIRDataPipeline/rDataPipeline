#' read_estimate
#'
#' Function to read point-estimate type data from toml file.
#'
#' @param filepath a \code{string} specifying the path and filename of the
#' file to be read
#'
#' @export
#'
#' @examples
#' # Write a single estimate into a toml file
#' filename <- "test.toml"
#'
#' create_estimate(filename = filename,
#'                 path = ".",
#'                 parameters = list(asymptomatic_period = 192.0))
#'
#' read_estimate(filepath = filename)
#'
#' file.remove(filename)
#'
#' # Write multiple estimates into a toml file
#' filename <- "anothertest.toml"
#'
#' create_estimate(filename = filename,
#'                 path = ".",
#'                 parameters = list(asymptomatic_period = 192.0,
#'                                   latent_period = 123.12))
#'
#' read_estimate(filepath = filename)
#'
#' file.remove(filename)
#'
read_estimate <- function(filepath) {
  # Read file
  tmp <- configr::read.config(file = filepath)

  # Check file
  if(tmp[[1]]$type != "point-estimate")
    stop("The file you are trying to read does not contain a point-estimate")

  tmp
}
