#' github_files
#'
#' @param repo *e.g.*
#'
#' @examples
#' \donttest{
#' \dontrun{
#' filelist <- list_files_in_repo("ScottishCovidResponse/SCRCdata")
#' files <- grep("scotgov_deaths.R$", filelist, value = TRUE)
#' }}
#'
github_files <- function(repo) {
  req <- httr::GET(paste("https://api.github.com/repos", repo,
                         "git/trees/master?recursive=1", sep = "/"))
  httr::stop_for_status(req)
  unlist(lapply(httr::content(req)$tree, "[", "path"), use.names = F)
}