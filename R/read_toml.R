#' read_toml
#'
#' @param filename a \code{string} specifying the name of the toml file
#'
#' @export
#'
read_toml <- function(filename) {
  tmp <- configr::read.config(file = filename)
  what <- tmp[[1]]$type

  if(grepl("distribution", what)) {
    output <- tmp[[1]]
    output$descriptor <- names(tmp)

  }else if (grepl("point-estimate", what)) {
    output <- setNames(as.numeric(tmp[[1]]$value), names(tmp))

  }
  output
}
