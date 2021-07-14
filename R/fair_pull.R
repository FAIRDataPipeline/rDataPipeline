#' fair_pull
#'
#' @param path path
#'
#' @export
#'
fair_pull <- function(path = "config.yaml") {

  if (grepl("localhost", endpoint)) run_server()

    # Read config.yaml --------------------------------------------------------

  if (file.exists(path)) {
    yaml <- yaml::read_yaml(path)
    usethis::ui_info(paste("Reading", usethis::ui_value("config.yaml")))
  } else
    usethis::ui_stop(paste(usethis::ui_value(path), "does not exist"))

  endpoint <- yaml$run_metadata$local_data_registry_url

  # Download any data required by `read:` from the remote data store and record
  # metadata in the data registry -------------------------------------------



  # Pull data associated with all previous versions of these objects from the
  # remote data registry ----------------------------------------------------



  # Download any data listed in register: from the original source and record
  # metadata in the data registry -------------------------------------------

  # Check for presence of `register` key

  if (any("register" %in% names(yaml))) {
    register <- yaml$register

    # If names(register) is null, then a single entry has been added that is
    # not in a list, so put it in a list
    if (!all(is.null(names(register))))
      register <- list(register)

    # Find entry to register
    types <- "external_object"
    entries <- lapply(register, function(x)
      lapply(types, function(y) x[[y]]) %>% unlist())

    # Download raw data to data store and register in data registry
    for (i in seq_along(register)) {
      register_external_object(yaml = yaml,
                               register_this = register[[i]],
                               endpoint = endpoint)
    }
  }

}
