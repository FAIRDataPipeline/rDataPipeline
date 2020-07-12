#' download_from_db
#'
#' @param url url
#' @param path path
#' @param local local
#' @param filename filename
#'
download_from_db <- function(url,
                             path,
                             local,
                             filename) {

  require(SPARQL, quietly = TRUE)

  # Prepare local directory -------------------------------------------------

  # Extract directory structure
  directory.structure <- strsplit(local, "/")[[1]]
  levels <- length(directory.structure)

  for(i in seq_along(directory.structure)) {
    if(i == 1) directory <- directory.structure[1]

    # If the directory doesn't exist then create it
    if(!file.exists(directory)) dir.create(directory)

    # Identify the next level to be generated
    if(i %in% seq_along(directory.structure)[-levels])
      directory <- file.path(directory, directory.structure[i+1])
  }

  # Save file ---------------------------------------------------------------

  # Download file
  dat <- SPARQL::SPARQL(url, path)$results

  # Save file
  write.csv(dat, file.path(local, filename))
}
