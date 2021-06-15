#' Clean query
#'
#' Function to clean a query and return it without an api prefix
#'
#' @param data a \code{list} containing a valid query for the table, *e.g.*
#' \code{list(field = value)}
#'
#' @export
#' @keywords internal
#'
clean_query <- function(data) {

  data_tmp <- lapply(data, function(x) {
    if (!is.character(x)) {
      output <- x

    } else if (grepl("^http://localhost:8000/api/.*([0-9]+$|[0-9]+/$)", x)) {
      output <- basename(x)

    } else {
      output <- x
    }

    output
  })

  # if(any(data_tmp == ""))
  #   data_tmp[[which(data_tmp == "")]] <- NULL

  data_tmp
}
