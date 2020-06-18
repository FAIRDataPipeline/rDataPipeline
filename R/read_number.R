#' read_number
#'
#' @export
#'
read_number <- function(toml_filename) {
  # Check file is a toml
  configr::is.toml.file(toml_filename)
  # Get section names
  configr::eval.config.sections(toml_filename)
  # Read file
  configr::read.config(file = toml_filename)
}
