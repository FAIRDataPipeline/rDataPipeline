#' random_hash
#'
#' Generates a random hash
#'
#' @export
#'
random_hash <- function() {
  tmp <- format(Sys.time(), "%Y%m%d%H%M%S%OS3") %>%
    as.numeric() * stats::runif(1, 1, 1000000)

  tmp %>%
    as.character() %>%
    openssl::sha1()
}
