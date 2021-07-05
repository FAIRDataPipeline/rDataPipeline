#' write_config
#'
#' Generates (user generated) config.yaml files for unit tests. Use
#' \code{read_dataproduct()} and \code{write_dataproduct()} functions to
#' add read and write blocks.
#'
#' @param path
#' @param description
#' @param input_namespace
#' @param output_namespace
#'
#' @export
#'
#' @examples
#' \dontrun {
#' write_config("test_config/config.yaml",
#'              description = "test",
#'              input_namespace = "test_user",
#'              output_namespace = "test_user")
#' }
#'
write_config <- function(path,
                         description,
                         input_namespace,
                         output_namespace) {

  if (file.exists(path))
    usethis::ui_stop(paste(usethis::ui_field(path), "already exists"))

  # Generate run_metadata block
  run_metadata <- list(description = description,
                       local_data_registry_url = "https://localhost:8000/api/",
                       remote_data_registry_url = "https://data.scrc.uk/api/",
                       default_input_namespace = input_namespace,
                       default_output_namespace = output_namespace,
                       write_data_store = "test_datastore/",
                       local_repo = "local_repo",
                       script = "")

  # If directory doesn't exist, create it
  directory <- dirname(path)
  if (!dir.exists(directory))
    dir.create(directory, recursive = TRUE)

  # Write working config.yaml file
  yaml::write_yaml(list(run_metadata = run_metadata), file = path)
}
