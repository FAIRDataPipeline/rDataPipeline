#' get_github_hash
#'
#' @param repo e.g. "ScottishCovidResponse/SCRCdata"
#'
#' @export
#'
#' This function assumes git is installed and located in the
#' System PATH
#'
get_github_hash <- function(repo) {
  tmp <- system(paste0("git ls-remote -h https://github.com/", repo, ".git"),
         intern = TRUE)
  gsub("\trefs/heads/master", "", tmp)
}
