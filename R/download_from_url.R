#' download_from_url
#'
download_from_url <- function(url, path, store) {

  # Remove beginning or trailing slashes
  store <- gsub("^/*", "", store)
  store <- gsub("/*$", "", store)

  # Extract directory structure
  directory.structure <- strsplit(store, "/")[[1]]

  levels <- length(directory.structure)

  for(i in seq_along(directory.structure)) {
    if(i == 1) directory <- directory.structure[1]

    # If the directory doesn't exist then create it
    if(!file.exists(directory)) dir.create(directory)

    # Identify the next level to be generated
    if(i %in% seq_along(directory.structure)[-levels])
      directory <- file.path(directory, directory.structure[i+1])
  }

  # Extract filename
  filename <- strsplit(paste0("/", path), "/")[[1]]
  filename <- filename[length(filename)]

  # Download and save file
  download.file(url = file.path(url, path),
                destfile = file.path(directory, filename))

  # If file is zipped, unzip it and remove *.zip file
  if(grepl(".zip$", filename)) {
    unzip(file.path(directory, filename), exdir = directory)
    file.remove(file.path(directory, filename))
  }

}
