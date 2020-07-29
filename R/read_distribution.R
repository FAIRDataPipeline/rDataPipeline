#' read_distribution
#'
#' @param filename a \code{string} specifying the name of the toml file, e.g.
#' "0.1.0.toml"
#'
read_distribution <- function(filename) {
  tmp <- configr::read.config(file = filename)
  what <- tmp[[1]]$type

  output <- tmp[[1]]
  output$descriptor <- names(tmp)

  output
}
