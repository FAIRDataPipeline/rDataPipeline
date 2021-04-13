#' fdp_run
#'
#' @param yaml config.yaml
#' @param time time
#'
#' @export
#'
fdp_run <- function(yaml, time) {
  # If local data store doesn't exist, create it
  localstore <- yaml$run_metadata$default_data_store
  if (grepl("/$", localstore))
    localstore <- gsub("/$", "", localstore)

  # If config directory doesn't exist, create it
  configdir <- file.path(localstore, "config")
  if (!file.exists(configdir))
    dir.create(configdir)

  # Write .sh file (increment file name if it exists - why would it though?)
  filename <- paste0(time, ".yaml")
  filename <- increment_filename(configdir, filename)

  # Save working config.yaml in data store
  yaml::write_yaml(yaml, file = file.path(configdir, filename))

  # Save path in global environment
  Sys.setenv(wconfig = file.path("config", filename))
}
