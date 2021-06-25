#' get_token
#'
get_token <- function() {
  readLines(file.path("~", ".fair", "registry", "token"))
}
