#' fdp_run
#'
#' @param path string
#'
#' @export
#'
fdp_run <- function(path = "config.yaml") {
  yaml <- yaml::read_yaml(path)

  # If local data store doesn't exist, create it
  localstore <- yaml$run_metadata$default_data_store
  if (grepl("/$", localstore))
    localstore <- gsub("/$", "", localstore)

  # If config directory doesn't exist, create it
  configdir <- file.path(localstore, "config")
  if (!file.exists(configdir))
    dir.create(configdir)

  # Write .sh file (increment file name if it exists - why would it though?)
  filename <- paste0(format(Sys.time(), "%Y%m%d-%H%M%S"), ".yaml")
  filepath <- file.path(configdir, filename)
  filepath <- increment_filename(filepath)

  # Save working config.yaml in data store
  yaml::write_yaml(yaml, file = filepath)

  # Save path in global environment
  Sys.setenv(wconfig = filepath)
}
