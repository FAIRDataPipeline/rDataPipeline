#' initialise
#'
#' Reads in config.yaml file and generates new entry in code_run and returns id
#' and creates new submission_script in local registry and if necessary creates
#' a new code_repo entry in local registry.
#'
#' @param skip don't bother checking whether the repo is clean
#' @export
#'
initialise <- function(skip) {

  # Read config.yaml --------------------------------------------------------

  config_path <- Sys.getenv("wconfig")

  if (config_path == "")
    usethis::ui_stop(paste("Working", usethis::ui_value("config.yaml"),
                           "does not exist, please run `fdp pull` and try again"))

  yaml <- yaml::read_yaml(config_path)
  contents <- names(yaml)
  run_metadata <- yaml$run_metadata
  datastore_root <- yaml$run_metadata$default_data_store
  localstore <- run_metadata$default_data_store

  usethis::ui_done(paste("Read", usethis::ui_value("config.yaml")))







  fdp$new(yaml = yaml,
          model_config = config_object_id,
          submission_script = script_object_id)
}
