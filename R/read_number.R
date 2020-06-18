#' read_number
#'
#' @export
#'
read_number <- function(toml_filename) {
  configr::read.config(file = toml_filename)
}
