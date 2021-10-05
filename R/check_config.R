#' check_config
#'
#' @param handle an object of class \code{fdp, R6} containing metadata required
#' by the Data Pipeline API
#' @param data_product data_product
#' @param what what
#'
check_config <- function(handle, data_product, what) {

  error <- paste(usethis::ui_field(data_product), "missing from config file")

  # Get data products from {what} section
  dataproducts <- lapply(handle$yaml[[what]],
                         function(x) x$data_product) %>%
    unlist()

  # Check whether data product is listed in the {what} section
  config_match <- data_product %in% dataproducts

  if (config_match) {
    # If data product is listed in the {what} section, return 0
    return(invisible(0))

  } else {
    # If data product is not listed in the {what} section, check wildcards
    # matches
    any_wildcards <- lapply(dataproducts, function(x) grepl("\\*", x)) %>%
      unlist() %>%
      which()

    if (length(any_wildcards) == 0) {
      # If there aren't any wildcards, throw an error
      usethis::ui_stop(error)

    } else {
      wildcards <- dataproducts[[any_wildcards]]

      wildcard_match <- lapply(wildcards, function(x) {
        regex <- gsub("\\*$", "", x)
        grepl(regex, data_product)
      }) %>%
        unlist()

      if (any(wildcard_match)) {
        # If data product is listed in wildcards, return 0
        return(invisible(0))

      } else {
        # If data product is not listed in wildcards, throw an error
        usethis::ui_stop(error)
      }
    }
  }
}
