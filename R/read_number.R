#' read_number
#'
#' @param filename a \code{string} specifying the name of the toml file
#'
#' @export
#'
read_number <- function(filename) {
  tmp <- configr::read.config(file = filename)
  what <- tmp[[1]]$type

  setNames(as.numeric(tmp[[1]]$value), names(tmp))
}
