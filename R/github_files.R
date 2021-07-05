#' List files in GitHub repository
#'
#' @param repo a \code{string} specifying the github username/repository
#'
#' @keywords internal
#'
github_files <- function(repo) {
  req <- httr::GET(paste("https://api.github.com/repos", repo,
                         "git/trees/main?recursive=1", sep = "/"))
  if (req$status_code == 404) {
    req <- httr::GET(paste("https://api.github.com/repos", repo,
                           "git/trees/master?recursive=1", sep = "/"))
  }
  httr::stop_for_status(req)
  unlist(lapply(httr::content(req)$tree, "[", "path"), use.names = F)
}
