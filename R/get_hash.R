#' get_hash
#'
#' @param filename Filename
#'
#' @export
#'
get_hash <- function(filename) {
  file(filename) %>%
    openssl::sha1() %>%
    as.character()
}
