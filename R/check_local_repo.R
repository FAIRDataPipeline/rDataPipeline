#' check_local_repo
#' 
#' @keywords internal
#'
#' @param path Local repository file path
#' @return \code{boolean}, if local repository is clean (TRUE, else FALSE)
#'
check_local_repo <- function(path) {
  if (grepl("^~", path)) path <- gsub("^~", "", path)

  tmp <- git2r::status(path)

  if (length(tmp$staged) != 0)
    usethis::ui_oops(
      paste("Local repository is unclean, please check staged files:",
            usethis::ui_value(tmp$staged)))

  if (length(tmp$unstaged) != 0)
    usethis::ui_stop(
      paste("Local repository is unclean, please check unstaged files:",
            usethis::ui_value(tmp$unstaged)))

  if (length(tmp$untracked) != 0)
    usethis::ui_stop(
      paste("Local repository is unclean, please check untracked files:",
            usethis::ui_value(tmp$untracked)))

  invisible(all(lapply(tmp, length) == 0))
}
