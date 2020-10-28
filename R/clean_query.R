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
#' @examples
#' data <- list(storage_location = "https://data.scrc.uk/api/storage_location/2")
#' clean_query(data = data)
#'
clean_query <- function(data) {

  data_tmp <- lapply(data, function(x) {
    # There was a problem with ifelse() convering dates into numeric so filter
    # these out and use if_else()
    if(any(class(x) %in% c("POSIXct", "Date", "numeric"))) {
      output <- x
    } else {
      output <- dplyr::if_else(
        grepl("^https://data.scrc.uk/api/.*([0-9]+$|[0-9]+/$)", x),
        basename(x), x)
    }
    output
  })

  if(any(data_tmp == ""))
    data_tmp[which(data_tmp == "")] <- list(NULL)

  data_tmp
}
