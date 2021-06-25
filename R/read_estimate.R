#' Read estimate from TOML file
#'
#' Function to read point-estimate type data from toml file.
#'
#' @param filepath a \code{string} specifying the path and filename of the
#' file to be read
#'
#' @export
#'
read_estimate <- function(filepath) {
  # Read file
  tmp <- configr::read.config(file = filepath)

  # Check file
  if(tmp[[1]]$type != "point-estimate")
    stop("The file you are trying to read does not contain a point-estimate")

  tmp
}
