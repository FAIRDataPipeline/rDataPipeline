#' Get current GitHub hash
#'
#' Get the hash of the latest commit in the master branch of a particular
#' repository. This function assumes git is installed and located in the
#' System PATH.
#'
#' @param repo a \code{string} specifying the github username/repository
#'
#' @family get functions
#'
#' @export
#'
#' @examples
#' get_github_hash("ScottishCovidResponse/SCRCdata")
#'
get_github_hash <- function(repo) {
  if(system("git --version"))
    stop("git must be installed and located in the system path for this function to work")

  tmp <- system(paste0("git ls-remote -h https://github.com/", repo, ".git"),
         intern = TRUE)

  if(length(tmp) > 1) {
    ind <- which(grepl("master", tmp))
    tmp <- tmp[ind]
  }

  gsub("\trefs/heads/master", "", tmp)
}
