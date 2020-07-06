#' read_number
#'
#' @export
#'
read_number <- function(toml_filename) {
  tmp <- configr::read.config(file = toml_filename)$`point-estimate`
  setNames(as.numeric(tmp$value), tmp$name)
}
