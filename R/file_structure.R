#' file_structure
#'
#' Look at internal file structure
#'
#' @param filename filename
#'
#' @export
#'
file_structure <- function(filename) {

  file.h5 <- H5File$new(filename, mode = "r+")

  tmp <- file.h5$ls(recursive = TRUE) %>%
    dplyr::filter(grepl("array$|table$", name)) %>%
    dplyr::mutate(name = gsub("/array|/table", "", name)) %>%
    dplyr::select(name)

  file.h5$close_all()
  tmp
}
