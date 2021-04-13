#' store_script
#'
#' @param yaml config.yaml
#' @param time time
#'
#' @export
#'
store_script <- function(yaml, time) {
  x <- yaml$run_metadata
  # If local data store doesn't exist, create it
  localstore <- x$default_data_store
  if (grepl("/$", localstore))
    localstore <- gsub("/$", "", localstore)

  # If scripts directory doesn't exist, create it
  scriptdir <- file.path(localstore, "scripts")
  if (!file.exists(scriptdir))
    dir.create(scriptdir)

  # Write .sh file (increment file name if it exists - why would it though?)
  filename <- paste0(time, ".sh")
  filename <- increment_filename(scriptdir, filename)

  cat(x$script, file = file.path(scriptdir, filename))
  invisible(file.path("scripts", filename))
}
