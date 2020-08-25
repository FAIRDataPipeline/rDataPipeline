#' get_hash
#' Returns the SHA1 hash of a given file
#'
#' @param filename filename
#'
#' @export
#'
get_hash <- function(filename) {
  if(!file.exists(filename))
    stop(paste0("File ", filename, " does not exist"))
  file(filename) %>%
    openssl::sha1() %>%
    as.character() %>%
    as.character()
}
