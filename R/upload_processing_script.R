#' upload_processing_script
#'
#' @param storage_root
#'
upload_processing_script <- function(storage_root,
                                     path,
                                     hash,
                                     key) {

  # Check if storage_root exists (where processing script is stored)
  if(check_exists("storage_root", list(name = storage_root))) {
    storageRootId <- get_url("storage_root", list(name = storage_root))

  } else {
    stop(paste0("The storage_root \"", storage_root,
                "\" does not currently exist in the data repository.",
                "Please run new_storage_root() to write a new entry."))
  }

  # Check if storage_root exists (where processing script is stored)
  if(check_exists("storage_root", list(name = storage_root)) &
     check_exists("storage_location", list(name = path))) {
    stop("Git repository already exists.")
  }


  # upload processing script metadata to registry ---------------------------

  script_storeId <- new_storage_location(path = path,
                                         hash = "b4e5ff9b34092cdb3baf16f789546e325a2427a0",
                                         # system("git rev-parse HEAD",
                                         #             intern = TRUE),
                                         storage_root = storage_rootId,
                                         key = key)

  system("git ls-remote https://github.com/ScottishCovidResponse/SCRCdataAPI.git refs/heads/master")

  # git ls-remote -h https://github.com/ScottishCovidResponse/SCRCdataAPI.git
  git2r::repo("https://github.com/ScottishCovidResponse/SCRCdata/")

  git2r::commits(repo = git2r::remote_url(repo=))


  system("curl https://github.com/ScottishCovidResponse/SCRCdata/")



  new_object(storage_location = script_storeId,
             key = key)
}
