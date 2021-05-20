#' link_read
#'
#' @param handle an object of class \code{fdp, R6} containing metadata required
#' by the Data Pipeline API
#' @param alias a \code{string} representing an external object in the
#' config.yaml file
#'
#' @export
#'
link_read <- function(handle, alias) {

  # If the alias is already recorded in the handle, return the path
  if (alias %in% handle$inputs$alias) {
    output <- handle$inputs %>% dplyr::filter(.data$alias == alias) %>%
      dplyr::select(.data$path) %>%
      unlist() %>%
      unname()

    return(invisible(output))
  }

  # Get object metadata from working config.yaml
  read <- handle$yaml$read
  index <- lapply(read, function(x)
    alias == x$external_object) %>% unlist() %>% which()
  this_read <- read[[index]]

  # Get object location from local registry
  run_server()

  doi <- this_read$doi_or_unique_name
  external_object <- get_entry("external_object",
                               list(doi_or_unique_name = doi,
                                    title = this_read$title,
                                    version = this_read$version))
  assertthat::assert_that(length(external_object) == 1)
  external_object <- external_object[[1]]

  object <- get_entity(external_object$object)
  storage_location <- get_entity(object$storage_location)
  storage_root <- get_entity(storage_location$storage_root)

  stop_server()

  usethis::ui_info(paste0("Locating ", usethis::ui_value(doi), ": ",
                          usethis::ui_value(this_read$title), ", version ",
                          usethis::ui_value(this_read$version)))

  # Store metadata in handle
  handle$input(alias = alias,
               type = "external_object",
               doi_or_unique_name = this_read$doi_or_unique_name,
               title = this_read$title,
               version = this_read$version,
               path = paste0(storage_root$root, storage_location$path),
               object_id = unlist(object$components))

  # Return storage location
  invisible(paste0(storage_root$root, storage_location$path))
}
