#' read_distribution
#'
#' @param filename a \code{string} specifying the name of the toml file
#'
#' @export
#'
read_distribution <- function(filename) {
  tmp <- configr::read.config(file = filename)
  output <- tmp[[1]]
  output$descriptor <- names(tmp)
  output
}
