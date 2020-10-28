#' github_files
#'
#' @param repo a \code{string} specifying the github username/repository
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' filelist <- github_files("ScottishCovidResponse/SCRCdata")
#' files <- grep("scotgov_deaths.R$", filelist, value = TRUE)
#' }
#'
github_files <- function(repo) {
  req <- httr::GET(paste("https://api.github.com/repos", repo,
                         "git/trees/master?recursive=1", sep = "/"))
  httr::stop_for_status(req)
  unlist(lapply(httr::content(req)$tree, "[", "path"), use.names = F)
}
