#' get_index
#'
#' @param write write
#' @param data_product data_product
#'
get_index <- function(write, data_product) {
  index <- lapply(write, function(x)
    data_product == x$data_product) %>%
    unlist() %>%
    which()

  if (length(index) == 0) {
    glob_index <- lapply(write, function(x)
      basename(x$data_product) == "*") %>%
      unlist() %>%
      which()

    if (length(glob_index) == 0) {
      usethis::ui_stop("{data_product} not listed in config.yaml")

    } else {
      sub_index <- lapply(glob_index, function(y)
        grepl(dirname(write[[y]]$data_product), data_product)) %>%
        unlist() %>%
        which()
      index <- glob_index[sub_index]
      return(index)
    }
  } else {
    return(index)
  }
}
