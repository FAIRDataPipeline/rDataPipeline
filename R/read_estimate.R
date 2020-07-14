#' read_estimate
#'
#' @param filename a \code{string} specifying the name of the toml file, e.g.
#' "0.1.0.toml"
#'
#' @export
#'
read_estimate <- function(filename) {
  tmp <- configr::read.config(file = filename)
  what <- tmp[[1]]$type

  setNames(as.numeric(tmp[[1]]$value), names(tmp))
}
