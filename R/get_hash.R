#' get_hash
#'
#' @param filename filename
#'
#' @export
#'
get_hash <- function(filename) {
    file(filename) %>%
    openssl::sha1() %>%
    as.character()
}
