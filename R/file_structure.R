#' file_structure
#'
#' Look at internal file structure
#'
#' @export
#'
file_structure <- function(h5filename) {
  file.h5 <- H5File$new(h5filename, mode = "r+")

  tmp <- file.h5$ls(recursive = TRUE) %>%
    dplyr::filter(grepl("array$", name)) %>%
    dplyr::mutate(name = gsub("/array", "", name)) %>%
    dplyr::select(name)

  file.h5$close_all()
  tmp
}
