#' clean_query
#'
#' Function to return clean a query and return it without api prefix
#'
#' *e.g*
#'
#' \code{clean_query(data = list(storage_location = "https://data.scrc.uk/api/storage_location/2"))}
#'
#' will return:
#'
#' \code{$storage_location}
#'
#' \code{[1] "2"}
#'
#' @param data data typically from a query
#' *e.g* \code{data = list(storage_location = "https://data.scrc.uk/api/storage_location/2")}
#'
#' @export
#'
clean_query <- function(data) {
  data_tmp <- lapply(data, function(x) {
    ifelse(grepl("^https://data.scrc.uk/api/.*[0-9]+/$", x), basename(x), x)
  })
  if(any(data_tmp == "")) {
    data_tmp[which(data_tmp == "")] <- list(NULL)
  }

  return(data_tmp)
}
