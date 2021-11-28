#' get_token
#'
#' @noRd
#'
get_token <- function() {
  readLines(file.path("~", ".fair", "registry", "token"))
}
