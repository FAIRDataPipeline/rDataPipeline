#' create_config
#'
#' Generates (user generated) config.yaml files for unit tests. Use
#' \code{add_read()} and \code{add_write()} functions to add read and write
#' blocks.
#'
#' @param path config file path
#' @param description description field
#' @param input_namespace input_namespace field
#' @param output_namespace output_namespace field
#' @param write_data_store write_data_store field
#' @param local_repo local_repo
#' @param force force
#'
#' @export
#'
create_config <- function(path,
                          description,
                          input_namespace,
                          output_namespace,
                          write_data_store = file.path(tempdir(),
                                                       "datastore", ""),
                          force = TRUE,
                          local_repo = "local_repo") {

  if (file.exists(path)) {
    if (force) {
      file.remove(path)
    } else {
      usethis::ui_stop(paste(usethis::ui_field(path), "already exists"))
    }
  }

  # Generate run_metadata block
  run_metadata <- list(description = description,
                       local_data_registry_url = "http://localhost:8000/api/",
                       remote_data_registry_url = "https://data.scrc.uk/api/",
                       default_input_namespace = input_namespace,
                       default_output_namespace = output_namespace,
                       write_data_store = write_data_store,
                       local_repo = local_repo,
                       script = "")

  # If directory doesn't exist, create it
  directory <- dirname(path)
  if (!dir.exists(directory))
    dir.create(directory, recursive = TRUE)

  # Write working config.yaml file
  yaml::write_yaml(list(run_metadata = run_metadata), file = path)
}
