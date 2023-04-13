#' Get H5 file components
#'
#' Returns the names of the items at the root of the file
#'
#' @param filename a \code{string} specifying a filename
#'
#' @return Returns the names of the items at the root of the file
#'
#' @family get functions
#'
#' @export
#'
get_components <- function(filename) {

  if (!file.exists(filename))
    stop("File does not exist")

  tmp <- rhdf5::H5Fopen(filename) %>%
    h5ls() %>%
    dplyr::filter(.data$name == "array" | .data$name == "table") %>%
    dplyr::select(.data$group) %>%
    unlist() %>%
    unname()

  rhdf5::h5closeAll()

  gsub("^/", "", tmp)
}
