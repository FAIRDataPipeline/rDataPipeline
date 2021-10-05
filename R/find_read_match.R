#' Find matching read aliases in config file
#'
#' Find read aliases in working config that match wildcard string
#'
#' @param handle an object of class \code{fdp, R6} containing metadata required
#' by the Data Pipeline API
#' @param data_product a \code{string} specifying the data product name
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
