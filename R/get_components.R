#' get_components
#'
#' Returns the names of the items at the root of the file
#'
#' @param filename filename
#'
#' @export
#'
get_components <- function(filename) {

  tmp <- rhdf5::H5Fopen(filename) %>%
    h5ls() %>%
    dplyr::filter(.data$name == "array" | .data$name == "table") %>%
    dplyr::select(.data$group) %>%
    unlist() %>%
    unname()

  rhdf5::h5closeAll()

  gsub("^/", "", tmp)
}