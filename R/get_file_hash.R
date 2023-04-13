#' Calculate hash from file
#'
#' Returns the SHA1 hash of a given file
#' 
#' @keywords internal
#'
#' @param filename a \code{string} specifying a filename
#'
#' @family get functions
#'
get_file_hash <- function(filename) {
  if (!file.exists(filename))
    stop(paste0("File ", filename, " does not exist"))
  file(filename) %>%
    openssl::sha1() %>%
    as.character() %>%
    as.character()
}
