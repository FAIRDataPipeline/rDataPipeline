#' file_structure
#'
#' Returns the names of the items at the root of the file
#'
#' @param filename filename
#'
#' @export
#'
file_structure <- function(filename) {

  tmp <- rhdf5::H5Fopen(filename) %>%
    h5ls() %>%
    dplyr::filter(name == "array" | name == "table") %>%
    dplyr::select(group) %>%
    unlist() %>%
    unname()

  rhdf5::h5closeAll()

  gsub("^/", "", tmp)
}
