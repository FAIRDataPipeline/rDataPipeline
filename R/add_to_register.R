#' add_to_register
#'
#' Register externally sourced data to the pipeline.
#'
#' @param handle list
#' @param alias string
#'
#' @export
#'
add_to_register <- function(handle, alias) {
  yaml <- handle$yaml
  datastore <- yaml$run_metadata$default_data_store
  namespace <- yaml$run_metadata$default_output_namespace

  # Check for presence of `register` key
  if (!any("register" %in% names(yaml)))
    usethis::ui_stop(paste("Missing section in config.yaml file,",
                           usethis::ui_value("register:")))

  register <- yaml$register

  # If names(register) is null, then a single entry has been added that is
  # not in a list, so put it in a list
  if (!all(is.null(names(register))))
    register <- list(register)

  # Find entry to register
  types <- c("external_object", "data_product", "object")
  entries <- lapply(register, function(x)
    lapply(types, function(y) x[[y]]) %>% unlist())

  # If entries[[1]] is null, then register doesn't contain one of
  # `external_object`, `data_product`, or `object`
  if(is.null(entries[[1]]))
    usethis::ui_stop(paste("Missing section in config.yaml file,",
                           usethis::ui_value("register:"),
                           "must contain one of",
                           usethis::ui_value("external_object:"),
                           usethis::ui_value("data_product:"), "or",
                           usethis::ui_value("object:")))

  # Register this entry
  index <- which(entries == alias)
  register_this <- register[[index]]

  if ("external_object" %in% names(register_this)) {
    externalobject_id <- register_external_object(register_this,
                                                  datastore = datastore,
                                                  namespace = namespace)

    handle$input(alias, externalobject_id)
  }
}
