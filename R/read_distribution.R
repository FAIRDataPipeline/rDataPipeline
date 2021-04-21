#' Read distribution from TOML file
#'
#' Function to read disdtribution type data from toml file.
#'
#' @param filepath a \code{string} specifying the path and filename of the file
#' to be read
#'
#' @export
#'
read_distribution <- function(filepath) {
  # Read file
  tmp <- configr::read.config(file = filepath)

  # Check file
  if(tmp[[1]]$type != "distribution")
    stop("The file you are trying to read does not contain a distribution")

  tmp
}
