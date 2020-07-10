#' create_version_number
#'
#' @param download_date
#' @param version
#'
create_version_number <- function(download_date, version) {
  tmp <- gsub(" [0-9]*:[0-9]*:[0-9]*$", "", download_date)
  tmp <- gsub("-", "", tmp)
  paste0(tmp, ".", version)
}
