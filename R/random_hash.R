#' random_hash
#'
#' Generates a random hash
#'
random_hash <- function() {
  openssl::sha1(as.character(Sys.time()))
}
