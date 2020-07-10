#' upload_processing_script
#'
#'
#'
upload_processing_script <- function() {
  storage_rootId <- new_storage_root(name = "github",
                                     root = "https://github.com",
                                     accessibility = "public")

  script_storeId <- new_storage_location(path = gitrepo,
                                         hash = system("git rev-parse HEAD",
                                                       intern = TRUE),
                                         storage_root = storage_rootId)

  new_object(storage_location = script_storeId)
}
