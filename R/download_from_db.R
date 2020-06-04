#' download_from_db
#'
download_from_db <- function(url, path, store) {

  # Download file
  dat <- SPARQL::SPARQL(url, path)$results

  # Extract filename
  filename <- "deaths-involving-coronavirus-covid-19.csv"

  # Save file
  write.csv(dat, file.path(store, filename))
}
