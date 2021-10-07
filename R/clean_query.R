#' Clean query
#'
#' Function to clean a query and return it without an api prefix
#'
#' @param data a \code{list} containing a valid query for the table, *e.g.*
#' \code{list(field = value)}
#' @param endpoint endpoint
#'
clean_query <- function(data, endpoint) {

  data_tmp <- lapply(data, function(x) {
    if (!is.character(x)) {
      output <- x

    } else if (grepl(paste0(endpoint, ".*([0-9]+$|[0-9]+/$)"), x)) {
      output <- basename(x)

    } else {
      output <- x
    }

    output
  })

  data_tmp
}
