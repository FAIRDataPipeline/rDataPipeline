#' Get current GitHub hash
#'
#' Get the hash of the latest commit in the master branch of a particular
#' repository. This function assumes git is installed and located in the
#' System PATH.
#' 
#' @keywords internal
#'
#' @param repo a \code{string} specifying the github username/repository
#'
#' @family get functions
#'
get_github_hash <- function(repo) {
  if (!grepl("version", system("git --version", intern = TRUE))) {
    msg <- paste("git must be installed and located in the system path",
                 "for this function to work")
    stop(msg)
  }

  tmp <- system(paste("git ls-remote -h", repo), intern = TRUE)

  strsplit(tmp, "\\\\|[^[:print:]]")[[1]][1]
}
