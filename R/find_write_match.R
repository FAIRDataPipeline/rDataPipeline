#' Find matching write aliases in config file
#'
#' Find write aliases in working config that match wildcard string
#'
#' @param handle an object of class \code{fdp, R6} containing metadata required
#' by the Data Pipeline API
#' @param data_product a \code{string} specifying the data product name
#'
#' @export
#'
find_write_match <- function(handle, data_product) {
  working_config_writes <- handle$yaml$write %>%
    lapply(function(x) x$data_product) %>%
    unlist()

  regex <- gsub("\\*$", "", data_product)
  index <- grep(regex, working_config_writes)
  working_config_writes[index]
}
