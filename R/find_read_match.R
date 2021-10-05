#' find_read_match
#'
#' Find aliases in working config that match wildcard string
#'
#' @param handle an object of class \code{fdp, R6} containing metadata required
#' by the Data Pipeline API
#' @param data_product data_product
#'
#' @export
#'
find_read_match <- function(handle, data_product) {
  working_config_reads <- handle$yaml$read %>%
    lapply(function(x) x$data_product) %>%
    unlist()

  regex <- gsub("\\*$", "", data_product)
  index <- grep(regex, working_config_reads)
  working_config_reads[index]
}
